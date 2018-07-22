module Main (main) where

import Data.ByteString.Char8 (ByteString)
import System.Posix.Types (GroupID, UserID, FileOffset, Fd, ByteCount)
import System.Posix.Files
    (
        unionFileModes,
        otherExecuteMode,
        otherReadMode,
        groupExecuteMode,
        groupReadMode,
        ownerExecuteMode,
        ownerReadMode
    )
import System.Posix.IO (OpenMode, OpenFileFlags, closeFd, openFd)
import "unix-bytestring" System.Posix.IO.ByteString (fdPread, fdPwrite)
import Foreign.C.Error (Errno, eOK, eNOENT, eNOTDIR, eNOSYS, eINVAL)
import System.Fuse
    (
        fuseMain,
        fuseGetFileSystemStats,
        fuseReadDirectory,
        fuseOpenDirectory,
        fuseFlush,
        fuseWrite,
        fuseRead,
        fuseOpen,
        fuseReadSymbolicLink,
        fuseGetFileStat,
        defaultFuseOps,
        FuseOperations
    )
import qualified System.Fuse as Fuse
import Data.Map.Lazy (Map)
import qualified Data.Map.Strict as Map
import System.FilePath (splitPath)
import Data.List (dropWhileEnd)
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID)
import Data.Either (fromLeft)

data Handle = External Fd | Internal
data TagTree = Link FilePath | Dir (Map FilePath TagTree) deriving Show

{- TODO

Find all tags files and build a list of tags for each file. Each file is reached via a path in `concat (subsequences <$> permutation paths)`.
Combine all paths to make a `TagTree`.

To avoid conflicts, we have a directory for sub tags. The directory is called `and`, `or` or `not`, corresponding to the logical operator.

It may be better to view this as building an expression, where it's not useful to show tautologies like `X and X`.
Tautologies may be the wrong concept - maybe we want the concept of something not narrowing down the data set.
-}
dirTree :: TagTree
dirTree = Dir $ Map.fromList [("simon", Link "/home/simon"), ("test", Dir $ Map.fromList [("xyz", Link "/home/simon/bin/BourneoDB.sh")])]

lookupPath :: FilePath -> TagTree -> Maybe TagTree
lookupPath path =
    let
        removeTrailingSlashes = dropWhileEnd (== '/')
        removeRoot = filter (/= "/")

        go fp tree = case (tree, fp) of
            (_, []) -> Just tree
            (Dir entries, name : tailPath) -> Map.lookup (removeTrailingSlashes name) entries >>= go tailPath
            _ -> Nothing
    in go (removeRoot $ splitPath path)

getEntryType :: TagTree -> Fuse.EntryType
getEntryType = \case
    Dir _ -> Fuse.Directory
    Link _ -> Fuse.SymbolicLink

ifExists :: TagTree -> FilePath -> (TagTree -> IO (Either Errno a)) -> IO (Either Errno a)
ifExists tree fp f =
    let path = lookupPath fp tree
    in maybe (pure $ Left eNOENT) f path

getFileStat :: MkStat -> TagTree -> FilePath -> IO (Either Errno Fuse.FileStat)
getFileStat (MkStat stat) tree fp = ifExists tree fp (pure . Right . stat . getEntryType)

readSymLink :: TagTree -> FilePath -> IO (Either Errno FilePath)
readSymLink tree fp =
    let
        symLinkDest :: TagTree -> IO (Either Errno FilePath)
        symLinkDest = pure . \case
            Link dest -> Right dest
            Dir _ -> Left eINVAL
    in ifExists tree fp symLinkDest

passThrough :: Handle -> (Fd -> IO a) -> IO (Either Errno a)
passThrough h f = case h of
    External fd -> Right <$> f fd
    Internal -> pure $ Left eNOSYS

openFile  :: TagTree -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Handle)
openFile tree fp mode flags =
    let
        openAt = \case
            Link dest -> (Right . External) <$> openFd dest mode Nothing flags
            Dir _ -> pure $ Right Internal
    in ifExists tree fp openAt

readExternalFile :: FilePath -> Handle -> ByteCount -> FileOffset -> IO (Either Errno ByteString)
readExternalFile _ h bc offset = passThrough h $ \fd -> fdPread fd bc offset

writeExternalFile ::  FilePath -> Handle -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
writeExternalFile _ h buf offset = passThrough h $ \fd -> fdPwrite fd buf offset

flushFile ::  FilePath -> Handle -> IO Errno
flushFile _ h = fromLeft eOK <$> passThrough h closeFd

openDirectory :: TagTree -> FilePath -> IO Errno
openDirectory tree fp = pure $ maybe eNOENT (const eOK) $ lookupPath fp tree

readDirectory :: MkStat -> TagTree -> FilePath -> IO (Either Errno [(FilePath, Fuse.FileStat)])
readDirectory (MkStat stat) tree path =
    let
        addStat = stat . getEntryType

        readIfDir :: TagTree -> IO (Either Errno [(FilePath, Fuse.FileStat)])
        readIfDir = pure . \case
            Link _ -> Left eNOTDIR
            Dir entries -> Right $ (fmap addStat <$> Map.toList entries)
    in ifExists tree path readIfDir

getFileSystemStats :: String -> IO (Either Errno Fuse.FileSystemStats)
getFileSystemStats = const $ pure $ Right $ Fuse.FileSystemStats
    {
        Fuse.fsStatBlockSize = 512,
        Fuse.fsStatBlockCount = 0,
        Fuse.fsStatBlocksFree = 0,
        Fuse.fsStatBlocksAvailable = 0,
        Fuse.fsStatFileCount = 0,
        Fuse.fsStatFilesFree = 0,
        Fuse.fsStatMaxNameLength = 255
    }

fsOps :: TagTree -> MkStat -> FuseOperations Handle
fsOps tree stat = defaultFuseOps
    {
        fuseGetFileStat = getFileStat stat tree,
        fuseReadSymbolicLink = readSymLink tree,
        fuseOpen = openFile tree,
        fuseRead = readExternalFile,
        fuseWrite = writeExternalFile,
        fuseFlush = flushFile,
        fuseOpenDirectory = openDirectory tree,
        fuseReadDirectory = readDirectory stat tree,
        fuseGetFileSystemStats = getFileSystemStats
    }

newtype MkStat = MkStat (Fuse.EntryType -> Fuse.FileStat)

mkStat :: UserID -> GroupID -> MkStat
mkStat userId groupId = MkStat $ \entryType -> Fuse.FileStat
    {
        Fuse.statEntryType = entryType,
        Fuse.statFileMode = foldr unionFileModes ownerReadMode
            [
                ownerExecuteMode,
                groupReadMode,
                groupExecuteMode,
                otherReadMode,
                otherExecuteMode
            ],
        Fuse.statLinkCount = 1,
        Fuse.statFileOwner = userId,
        Fuse.statFileGroup = groupId,
        Fuse.statSpecialDeviceID = 0,
        Fuse.statFileSize = 1,
        Fuse.statBlocks = 0,
        Fuse.statAccessTime = 0,
        Fuse.statModificationTime = 0,
        Fuse.statStatusChangeTime = 0
    }

main :: IO ()
main = do
    userId <- getEffectiveUserID
    groupId <- getEffectiveGroupID
    fuseMain (fsOps dirTree $ mkStat userId groupId) Fuse.defaultExceptionHandler

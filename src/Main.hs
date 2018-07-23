module Main (main) where

import ClassyPrelude
import Codec.Binary.UTF8.String(decode)
import Data.ByteString.Char8 (ByteString)
import System.FilePath.Find (fileType, fileName, FileType(..), (==?), (&&?))
import qualified System.FilePath.Find as File
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
import System.FilePath (takeFileName, takeDirectory, splitPath)
import Data.List (dropWhileEnd)
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID)
import Data.Either (fromLeft)
import System.Directory (makeAbsolute)

data TagHandle = External Fd | Internal
data TagTree = Link FilePath | Dir (Map FilePath TagTree) deriving Show

instance Semigroup TagTree where
    -- Favour links over directories, as links are results.
    x <> y = case (x, y) of
        (Link _, _) -> x
        (_, Link _) -> y
        (Dir entriesX, Dir entriesY) -> Dir $ Map.unionWith mappend entriesX entriesY

instance Monoid TagTree where
    mempty = Dir Map.empty
    mappend = (<>)

pathComponents :: FilePath -> [FilePath]
pathComponents path = filter (/= "") (dropWhileEnd (== '/') <$> splitPath path)

lookupPath :: FilePath -> TagTree -> Maybe TagTree
lookupPath path =
    let
        go fp tree = case (tree, fp) of
            (_, []) -> Just tree
            (Dir entries, name : tailPath) -> Map.lookup name entries >>= go tailPath
            _ -> Nothing
    in go $ pathComponents path

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

passThrough :: TagHandle -> (Fd -> IO a) -> IO (Either Errno a)
passThrough h f = case h of
    External fd -> Right <$> f fd
    Internal -> pure $ Left eNOSYS

openFile  :: TagTree -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno TagHandle)
openFile tree fp mode flags =
    let
        openAt = \case
            Link dest -> (Right . External) <$> openFd dest mode Nothing flags
            Dir _ -> pure $ Right Internal
    in ifExists tree fp openAt

readExternalFile :: FilePath -> TagHandle -> ByteCount -> FileOffset -> IO (Either Errno ByteString)
readExternalFile _ h bc offset = passThrough h $ \fd -> fdPread fd bc offset

writeExternalFile ::  FilePath -> TagHandle -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
writeExternalFile _ h buf offset = passThrough h $ \fd -> fdPwrite fd buf offset

flushFile ::  FilePath -> TagHandle -> IO Errno
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

fsOps :: TagTree -> MkStat -> FuseOperations TagHandle
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

data Tagged = Tagged FilePath [FilePath] deriving Show

readTags :: FilePath -> IO Tagged
readTags tagsFile = (Tagged tagsFile . filter (/= "") . lines . decode . unpack) <$> readFile tagsFile

prefixNonEmpty :: a -> [[a]] -> [a]
prefixNonEmpty x = \case
    [] -> []
    xs -> x : intercalate [x] xs

allPaths :: [FilePath] -> [[FilePath]]
allPaths tags = concat (((prefixNonEmpty "and" <$>) <$> permutations <$> subsequences (pathComponents <$> tags)))

singleTagTree :: FilePath -> [FilePath] -> TagTree
singleTagTree tagsFile =
    let dir = takeDirectory tagsFile
    in \case
        [] -> Dir $ Map.singleton (takeFileName dir) $ Link dir
        tag : tags -> Dir $ Map.singleton tag $ singleTagTree tagsFile tags

allTagTrees :: Tagged -> TagTree
allTagTrees (Tagged dir paths) = mconcat (singleTagTree dir <$> allPaths paths)

main :: IO ()
main = do
    tagsFiles <- File.find (fileType ==? Directory) (fileType ==? RegularFile &&? fileName ==? "tags") "."
    absTagsFiles <- mapM makeAbsolute tagsFiles
    tagsContents <- mapM readTags absTagsFiles

    let dirTree = mconcat (allTagTrees <$> tagsContents)
    userId <- getEffectiveUserID
    groupId <- getEffectiveGroupID
    fuseMain (fsOps dirTree $ mkStat userId groupId) Fuse.defaultExceptionHandler

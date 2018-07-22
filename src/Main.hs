module Main (main) where

import Data.ByteString.Char8 (ByteString)
import System.Posix.Types (GroupID, UserID, FileOffset, Fd, ByteCount)
import System.Posix.Files (otherExecuteMode, otherReadMode, groupExecuteMode, groupReadMode, ownerExecuteMode, ownerReadMode)
import System.Posix.IO (closeFd, openFd)
import "unix-bytestring" System.Posix.IO.ByteString (fdPread, fdPwrite)
import System.Fuse
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.FilePath (splitPath)
import Data.List (dropWhileEnd)
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID)
import Data.Either (fromLeft)

data Handle = External Fd | Internal
data TagTree = Link FilePath | Dir (Map FilePath TagTree) deriving Show

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

getEntryType :: TagTree -> EntryType
getEntryType = \case
    Dir _ -> Directory
    Link _ -> SymbolicLink

getFileStat :: MkStat -> TagTree -> FilePath -> IO (Either Errno FileStat)
getFileStat (MkStat stat) tree fp = pure $ case lookupPath fp tree of
    Just entry -> Right $ stat $ getEntryType entry
    Nothing -> Left eNOENT

readSymLink :: TagTree -> FilePath -> IO (Either Errno FilePath)
readSymLink tree fp = pure $ case lookupPath fp tree of
    Just (Link dest) -> Right dest
    Just (Dir _) -> Left eINVAL
    Nothing -> Left eNOENT

openFile  :: TagTree -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno Handle)
openFile tree fp mode flags = case lookupPath fp tree of
    Just (Link dest) -> (Right . External) <$> openFd dest mode Nothing flags
    Just (Dir _) -> pure $ Right Internal
    Nothing -> pure $ Left eNOENT

passThrough :: Handle -> (Fd -> IO a) -> IO (Either Errno a)
passThrough h f = case h of
    External fd -> Right <$> f fd
    Internal -> pure $ Left eNOSYS

readExternalFile :: FilePath -> Handle -> ByteCount -> FileOffset -> IO (Either Errno ByteString)
readExternalFile _ h bc offset = passThrough h $ \fd -> fdPread fd bc offset

writeExternalFile ::  FilePath -> Handle -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
writeExternalFile _ h buf offset = passThrough h $ \fd -> fdPwrite fd buf offset

flushFile ::  FilePath -> Handle -> IO Errno
flushFile _ h = fromLeft eOK <$> passThrough h closeFd

openDirectory :: TagTree -> FilePath -> IO Errno
openDirectory tree fp = pure $ maybe eNOENT (const eOK) $ lookupPath fp tree

readDirectory :: MkStat -> TagTree -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
readDirectory (MkStat stat) tree path =
    let addStat = stat . getEntryType
    in do
        print "readDirectory"
        pure $ case lookupPath path tree of
            Nothing -> Left eNOENT
            Just (Link _) -> Left eNOTDIR
            Just (Dir entries) -> Right $ (fmap addStat <$> Map.toList entries)

getFileSystemStats :: String -> IO (Either Errno FileSystemStats)
getFileSystemStats = const $ pure $ Right $ FileSystemStats
    {
        fsStatBlockSize = 512,
        fsStatBlockCount = 0,
        fsStatBlocksFree = 0,
        fsStatBlocksAvailable = 0,
        fsStatFileCount = 0,
        fsStatFilesFree = 0,
        fsStatMaxNameLength = 255
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

newtype MkStat = MkStat (EntryType -> FileStat)

mkStat :: UserID -> GroupID -> MkStat
mkStat userId groupId = MkStat $ \entryType -> FileStat
    {
        statEntryType = entryType,
        statFileMode = foldr unionFileModes ownerReadMode
            [
                ownerExecuteMode,
                groupReadMode,
                groupExecuteMode,
                otherReadMode,
                otherExecuteMode
            ],
            statLinkCount = 1,
            statFileOwner = userId,
            statFileGroup = groupId,
            statSpecialDeviceID = 0,
            statFileSize = 1,
            statBlocks = 0,
            statAccessTime = 0,
            statModificationTime = 0,
            statStatusChangeTime = 0
    }

main :: IO ()
main = do
    userId <- getEffectiveUserID
    groupId <- getEffectiveGroupID
    fuseMain (fsOps dirTree $ mkStat userId groupId) defaultExceptionHandler

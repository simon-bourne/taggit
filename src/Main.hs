module Main where

import Prelude hiding (readFile, writeFile, lookup)
import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import "unix-bytestring" System.Posix.IO.ByteString
import Foreign.C.Error
import System.Fuse
import Foreign.C.Types
import Data.Map.Strict (fromList, Map, keys, lookup, toList)
import System.FilePath
import Data.List (dropWhileEnd)
import System.Posix.User
import System.Posix.Types

data Handle = External Fd | Internal
data TagTree = Link FilePath | Dir (Map FilePath TagTree) deriving Show

lookupPath :: FilePath -> TagTree -> Maybe TagTree
lookupPath path =
    let
        removeTrailingSlash = dropWhileEnd (== '/')
        removeRoot = filter (/= "/")

        go fp tree = case (tree, fp) of
            (_, []) -> Just tree
            (Dir entries, name : tailPath) -> lookup (removeTrailingSlash name) entries >>= go tailPath
            _ -> Nothing
    in go (removeRoot $ splitPath path)

dirTree :: TagTree
dirTree = Dir $ fromList [("simon", Link "/home/simon"), ("test", Dir $ fromList [("xyz", Link "/home/simon/bin/BourneoDB.sh")])]

main :: IO ()
main = do
    userId <- getEffectiveUserID
    groupId <- getEffectiveGroupID
    let dirStat = mkDirStat userId groupId
    let linkStat = mkLinkStat userId groupId
    fuseMain (fsOps dirStat linkStat dirTree) defaultExceptionHandler

readFile :: FilePath -> Handle -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
readFile _ h bc offset = case h of
    External fd -> Right <$> fdPread fd bc offset
    Internal -> pure $ Left eNOSYS

writeFile ::  FilePath -> Handle -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
writeFile _ h buf offset = case h of
    External fd -> Right <$> fdPwrite fd buf offset
    Internal -> pure $ Left eNOSYS

flushFile ::  FilePath -> Handle -> IO Errno
flushFile _ h = case h of
    External fd -> closeFd fd >> pure eOK
    Internal -> pure eNOSYS

openDirectory :: TagTree -> FilePath -> IO Errno
openDirectory tree fp = pure eOK

readDirectory :: FileStat -> FileStat -> TagTree -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
readDirectory dirStat linkStat tree path = let
    addStat = \case
        Dir _ -> dirStat
        Link _ -> linkStat
    in do
        print "readDirectory"
        pure $ case lookupPath path tree of
            Nothing -> Left eNOENT
            Just (Link _) -> Left eNOTDIR
            Just (Dir entries) -> Right $ (fmap addStat <$> toList entries)

getFileSystemStats :: String -> IO (Either Errno FileSystemStats)
getFileSystemStats str =
  pure $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
}

getFileStat :: FileStat -> FileStat -> TagTree -> FilePath -> IO (Either Errno FileStat)
getFileStat dirStat linkStat tree fp = pure $ case lookupPath fp tree of
    Just (Dir _) -> Right dirStat
    Just (Link _) -> Right linkStat
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

fsOps :: FileStat -> FileStat -> TagTree -> FuseOperations Handle
fsOps dirStat linkStat tree = defaultFuseOps{
    fuseGetFileStat = getFileStat dirStat linkStat tree,
    fuseReadSymbolicLink = readSymLink tree,
    fuseOpen = openFile tree,
    fuseRead = readFile,
    fuseWrite = writeFile,
    fuseFlush = flushFile,
    fuseOpenDirectory = openDirectory tree,
    fuseReadDirectory = readDirectory dirStat linkStat tree,
    fuseGetFileSystemStats = getFileSystemStats
}

mkLinkStat :: UserID -> GroupID -> FileStat
mkLinkStat userId groupId = FileStat { statEntryType = SymbolicLink
                       , statFileMode = foldr1 unionFileModes
                                          [ symbolicLinkMode,
                                            ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 1
                       , statFileOwner = userId
                       , statFileGroup = groupId
                       , statSpecialDeviceID = 0
                       , statFileSize = 3
                       , statBlocks = 0
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

mkDirStat userId groupId = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = userId
                       , statFileGroup = groupId
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

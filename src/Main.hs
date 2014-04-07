module Main where

import Checker

import           Control.Applicative             hiding (many, (<|>))
import           Control.Exception ()
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Trans.Writer.Lazy ()
import           Control.Proxy
import           Control.Proxy.Trans.Writer
import           Data.Maybe
import           System.Directory
import           System.Environment              (getArgs)
import           System.FilePath.Posix
import           System.Process

import           S3Checksums
import           Utils

import qualified Data.Map                        as DM
import qualified System.IO.Strict                as S

go :: [String] -> IO ()
go ["--checkall",        path]                              = runProxy $ getRecursiveContents path >-> useD (\file -> runReaderT (checkStoredChecksum file) path >>= print)
go ["--computemissing",  path]                              = runProxy $ getRecursiveContents path >-> useD (\file -> runReaderT (computeChecksums    file) path >>= print)

go ["--show-locals-missing-checksum",  path]                = runProxy $ getRecursiveContents path >-> useD (\file -> runReaderT (checkStoredChecksumIsMissing file) path)
go ["--show-local-orphaned-checksums", path]                = runProxy $ getRecursiveContents (path </> ".md5sums") >-> useD (\file -> runReaderT (checkOrphanedChecksum file) path)


go ["--update-s3-cache"]                                    = updateS3Cache
go ["--fix-etags"]                                          = fixExtendedEtags

go ["--check-checksums-local-vs-s3", s3Prefix, localPath]   = reportMismatchingMd5sumsLocalVsS3 s3Prefix localPath
go ["--show-in-both", s3Prefix, localPath]                  = reportFilesInBoth s3Prefix localPath
go ["--show-in-local-but-not-s3", s3Prefix, localPath]      = reportFilesInLocalButNotS3 s3Prefix localPath
go ["--show-in-s3-but-not-local", s3Prefix, localPath]      = reportFilesInS3ButNotLocal s3Prefix localPath

go _ = do
    putStrLn "Usage:"
    putStrLn ""
    putStrLn "    checker --checkall        <local dir>"
    putStrLn "    checker --computemissing  <local dir>"
    putStrLn ""
    putStrLn "    checker --show-locals-missing-checksum   <local dir>"
    putStrLn "    checker --show-local-orphaned-checksums  <local dir>"
    putStrLn ""
    putStrLn "    checker --update-s3-cache"
    putStrLn "    checker --fix-etags"
    putStrLn ""
    putStrLn "    checker --show-in-both                    <s3 url> <local path>"
    putStrLn "    checker --show-in-local-but-not-s3        <s3 url> <local path>"
    putStrLn "    checker --show-in-s3-but-not-local        <s3 url> <local path>"
    putStrLn "    checker --check-checksums-local-vs-s3     <s3 url> <local path>"
    putStrLn ""

main :: IO ()
main = getArgs >>= go

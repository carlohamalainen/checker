import Control.Exception
import Control.Monad ( forM_, liftM, filterM )
import Control.Proxy
import Data.String.Utils
import System.Directory
import System.Environment ( getArgs )
import System.FilePath.Posix
import System.IO
import System.Process

import Control.Monad.Identity
import Control.Monad.Reader

import Debug.Trace

-- Read everything else available on a handle, and return the empty
-- string if we have hit EOF.
readRestOfHandle :: Handle -> IO String
readRestOfHandle handle = do
    ineof <- hIsEOF handle
    if ineof
        then return ""
        else do x <- hGetContents handle
                return x

-- Compute the checksum (here, the md5sum) of a file. On success
-- we return the checksum in Right, otherwise we return error output
-- in Left.

------------------ checkStoredChecksum :: FilePath -> ReaderT FilePath IO ()
computeChecksum :: FilePath -> ReaderT FilePath IO (Either String String)
computeChecksum fileName = do
    (Just hin, Just hout, Just herr, pid) <- liftIO $ createProcess (proc "md5sum" [fileName]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdout <- liftIO $ readRestOfHandle hout
    stderr <- liftIO $ readRestOfHandle herr

    case length stderr of 0 -> return $ Right (head $ words stdout)
                          _ -> return $ Left stderr

checksumFilename :: FilePath -> ReaderT FilePath IO FilePath
checksumFilename f = do
    topdir <- ask
    return $ topdir </> ".md5sums" </> ((dropWhile (== '/') (drop (length topdir) f)) ++ ".md5sum")

isChecksumMissing :: FilePath -> ReaderT FilePath IO Bool
isChecksumMissing f = do
    topdir <- ask
    fileName <- checksumFilename f
    e <- liftM not $ liftIO $ doesFileExist fileName
    return e

computeChecksums :: FilePath -> ReaderT FilePath IO ()
computeChecksums f = do
    topdir <- ask

    md5file <- checksumFilename f
    let dir = dropFileName md5file

    isMissing <- isChecksumMissing f

    if isMissing
        then do Right md5 <- computeChecksum f
                liftIO $ createDirectoryIfMissing True dir
                liftIO $ writeFile md5file (md5 ++ "\n")
                liftIO $ putStrLn $ f ++ " ==> " ++ md5
        else return ()


checkStoredChecksum :: FilePath -> ReaderT FilePath IO ()
checkStoredChecksum f = do
    topdir <- ask

    md5file <- checksumFilename f
    hasChecksum <- liftM not $ isChecksumMissing f

    if hasChecksum
        then do storedMD5sum         <- liftIO $ liftM rstrip $ readFile md5file
                blah <- computeChecksum f
                case blah of (Right computedMD5sum) -> if storedMD5sum == computedMD5sum
                                                        then liftIO $ putStrLn $ "ok " ++ f
                                                        else liftIO $ putStrLn $ "fail " ++ f ++ " " ++ storedMD5sum ++ " != " ++ computedMD5sum
                             (Left  error)          -> liftIO $ putStrLn $ "error: " ++ error
        else liftIO $ putStrLn $ "checksum missing: " ++ f


-- http://stackoverflow.com/questions/14259229/streaming-recursive-descent-of-a-directory-in-haskell/14261710#14261710
getRecursiveContents :: (Proxy p) => FilePath -> () -> Producer p FilePath IO ()
getRecursiveContents topPath () = runIdentityP $ do
  names <- lift $ getDirectoryContents topPath
  let properNames  = filter (`notElem` [".", "..", ".md5sums"]) names
  forM_ properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- lift $ doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path ()
      else respond path

go :: [String] -> IO ()
go ["--checkall",       path] = runProxy $ getRecursiveContents path >-> useD (\file -> runReaderT (checkStoredChecksum file) path)
go ["--computemissing", path] = runProxy $ getRecursiveContents path >-> useD (\file -> runReaderT (computeChecksums    file) path)
go _ = putStrLn "Usage: checker <--checkall|--computemissing> <dir>"

main :: IO ()
main = getArgs >>= go



import Control.Applicative hiding ((<|>),many)
import Control.Exception
import Control.Monad ( forM_, liftM, filterM )
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Proxy
import Control.Proxy.Trans.Writer
import System.Directory
import System.Environment ( getArgs )
import System.FilePath.Posix
import System.IO
import System.Process

import S3Checksums
import Utils

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as DM
import qualified System.IO.Strict as S

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

computeChecksumFilename :: FilePath -> ReaderT FilePath IO FilePath
computeChecksumFilename f = do
    topdir <- ask
    return $ topdir </> ".md5sums" </> ((dropWhile (== '/') (drop (length topdir) f)) ++ ".md5sum")

isChecksumMissing :: FilePath -> ReaderT FilePath IO Bool
isChecksumMissing f = do
    topdir <- ask
    fileName <- computeChecksumFilename f
    e <- liftM not $ liftIO $ doesFileExist fileName
    return e

computeChecksums :: FilePath -> ReaderT FilePath IO ()
computeChecksums f = do
    topdir <- ask

    md5file <- computeChecksumFilename f
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

    md5file <- computeChecksumFilename f
    hasChecksum <- liftM not $ isChecksumMissing f

    if hasChecksum
        then do storedMD5sum         <- liftIO $ liftM rstripNewline $ readFile md5file
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

-- Note on execWriterT/raiseK: http://ocharles.org.uk/blog/posts/2012-12-16-24-days-of-hackage-pipes.html
getRecursiveContentsList :: FilePath -> IO [FilePath]
getRecursiveContentsList path =
    execWriterT $ runProxy $ raiseK (getRecursiveContents path) >-> toListD >>= return

-- Read local md5sum info. Assumes that he filename
-- ends with ".md5sum"! FIXME
readMd5Info :: FilePath -> IO (FilePath, String)
readMd5Info path = do
    let baseName = reverse $ tail $ dropWhile (/= '.') (reverse path)
    mdvalue <- rstripNewline <$> S.readFile path -- need to be strict here, otherwise we get 'too many open files'
    return $ (baseName, mdvalue)

getLocalAndS3Report _s3Prefix _localMd5dir = do
    let s3Prefix = trimTrailingSlash _s3Prefix
    let path     = trimTrailingSlash _localMd5dir

    localMD5info <- DM.fromList <$> map (\(x, y) -> (trimPathPrefix path x, y)) <$> (getRecursiveContentsList path >>= mapM readMd5Info)

    sp <- s3Lines

    case sp of Right s -> do let s3MD5info = DM.fromList $ map (\x -> (trimPathPrefix s3Prefix $ s3Path x, s3Md5sum x)) s
                             let s3MD5infoFull = DM.map fromRight $ DM.filter isRight s3MD5info -- Right == only files that are fully transferred to S3
                             return $ Just $ (localMD5info, s3MD5infoFull)
               Left e -> do print e
                            return Nothing

-- Report files that are fully stored in both S3 and the local path.
reportFilesInBoth s3Prefix localPath = do
    let localMd5Dir = localPath </> ".md5sums"

    r <- getLocalAndS3Report s3Prefix localMd5Dir

    case r of Just (localMD5info, s3MD5infoFull) -> forM_ (DM.toList $ DM.intersection localMD5info s3MD5infoFull) (\x -> do putStrLn $ (snd x) ++ " " ++ (fst x))
              _ -> putStrLn "error :("

-- Report files that are fully stored in local but not on S3
reportFilesInLocalButNotS3 s3Prefix localPath = do
    let localMd5Dir = localPath </> ".md5sums"

    r <- getLocalAndS3Report s3Prefix localMd5Dir

    case r of Just (localMD5info, s3MD5infoFull) -> forM_ (DM.toList $ DM.difference localMD5info s3MD5infoFull) (\x -> do putStrLn $ (snd x) ++ " " ++ (fst x))
              _ -> putStrLn "error :("

-- Report files that are fully stored in S3 not locally
reportFilesInS3ButNotLocal s3Prefix localPath = do
    let localMd5Dir = localPath </> ".md5sums"

    r <- getLocalAndS3Report s3Prefix localMd5Dir

    case r of Just (localMD5info, s3MD5infoFull) -> forM_ (DM.toList $ DM.difference s3MD5infoFull localMD5info) (\x -> do putStrLn $ (snd x) ++ " " ++ (fst x))
              _ -> putStrLn "error :("

go :: [String] -> IO ()
go ["--checkall",        path]                          = runProxy $ getRecursiveContents path >-> useD (\file -> runReaderT (checkStoredChecksum file) path)
go ["--computemissing",  path]                          = runProxy $ getRecursiveContents path >-> useD (\file -> runReaderT (computeChecksums    file) path)
go ["--update-s3-cache"]                                = updateS3Cache
go ["--show-in-both", s3Prefix, localPath]              = reportFilesInBoth s3Prefix localPath
go ["--show-in-local-but-not-s3", s3Prefix, localPath]  = reportFilesInLocalButNotS3 s3Prefix localPath
go ["--show-in-s3-but-not-local", s3Prefix, localPath]  = reportFilesInS3ButNotLocal s3Prefix localPath
go _ = do
    putStrLn "Usage:"
    putStrLn ""
    putStrLn "    checker --checkall        <local dir>"
    putStrLn "    checker --computemissing> <local dir>"
    putStrLn ""
    putStrLn "    checker --update-s3-cache"
    putStrLn ""
    putStrLn "    checker --show-in-both             <s3 url> <local path>"
    putStrLn "    checker --show-in-local-but-not-s3 <s3 url> <local path>"
    putStrLn "    checker --show-in-s3-but-not-local <s3 url> <local path>"
    putStrLn ""

main :: IO ()
main = getArgs >>= go

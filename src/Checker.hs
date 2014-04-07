module Checker ( computeChecksum
               , computeChecksums
               , checkStoredChecksum
               , isMatch
               , validCharacters
               , getRecursiveContents
               , checkStoredChecksumIsMissing
               , checkOrphanedChecksum
               , updateS3Cache
               , fixExtendedEtags
               , reportMismatchingMd5sumsLocalVsS3
               , reportFilesInBoth
               , reportFilesInLocalButNotS3
               , reportFilesInS3ButNotLocal
               ) where

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

-- Compute the checksum (here, the md5sum) of a file. On success
-- we return the checksum in Right, otherwise we return error output
-- in Left.

validCharacters :: [Char]
validCharacters = " +-./0123456789ABCDFGIJLMNOPRSUV_abcdefghijklmnoprstuvwxy~"

isValidPath :: FilePath -> Bool
isValidPath = all (`elem` validCharacters)

computeChecksum :: FilePath -> IO (Either String String)
computeChecksum fileName = do
    (Just _, Just hout, Just herr, _) <- liftIO $ createProcess (proc "md5sum" [fileName]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdOut <- liftIO $ readRestOfHandle hout
    stdErr <- liftIO $ readRestOfHandle herr

    case length stdErr of 0 -> return $ Right (head $ words stdOut)
                          _ -> return $ Left stdErr

computeChecksumFilename :: FilePath -> ReaderT FilePath IO FilePath
computeChecksumFilename f = do
    unless (isValidPath f) $ error $ "Path contains invalid characters: " ++ f

    topdir <- ask
    let cf = topdir </> ".md5sums" </> (dropWhile (== '/') (drop (length topdir) f) ++ ".md5sum")

    when (255 < maximum (map length $ splitDirectories cf)) $ error $ "File is too long to construct md5sum filename: " ++ f

    return cf

isChecksumMissing :: FilePath -> ReaderT FilePath IO Bool
isChecksumMissing f = do
    fileName <- computeChecksumFilename f
    liftM not $ liftIO $ doesFileExist fileName

data ComputeResult = ComputeSkipped FilePath
                   | ComputeCreated FilePath String
                   deriving Eq

instance Show ComputeResult where
    show (ComputeSkipped f) = "skipped: " ++ f
    show (ComputeCreated f md5) = f ++ " ==> " ++ md5

computeChecksums :: FilePath -> ReaderT FilePath IO ComputeResult
computeChecksums f = do
    md5file <- computeChecksumFilename f
    let dir = dropFileName md5file

    isMissing <- isChecksumMissing f

    if isMissing
        then do
            Right md5 <- liftIO $ computeChecksum f
            liftIO $ createDirectoryIfMissing True dir
            liftIO $ writeFile md5file (md5 ++ "\n")
            return $ ComputeCreated f md5
        else return $ ComputeSkipped f

data ChecksumResult = ChecksumMatches FilePath
                    | ChecksumDiff FilePath String String
                    | ChecksumError FilePath String
                    | ChecksumMissing FilePath
                    deriving Eq

isMatch :: ChecksumResult -> Bool
isMatch (ChecksumMatches _) = True
isMatch _ = False

instance Show ChecksumResult where
    show (ChecksumMatches f)                = "ok: " ++ f
    show (ChecksumDiff f stored computed)   = "fail " ++ f ++ " " ++ stored ++ " != " ++ computed
    show (ChecksumError f e)                = "error computing checksum for: " ++ f ++ " " ++ e
    show (ChecksumMissing f)                = "checksum missing: " ++ f

checkStoredChecksum :: FilePath -> ReaderT FilePath IO ChecksumResult
checkStoredChecksum f = do
    md5file <- computeChecksumFilename f
    hasChecksum <- liftM not $ isChecksumMissing f

    if hasChecksum
        then do storedMD5sum         <- liftIO $ liftM rstripNewline $ readFile md5file
                blah <- liftIO $ computeChecksum f
                return (case blah of (Right computedMD5sum) -> if storedMD5sum == computedMD5sum
                                                                    then ChecksumMatches f
                                                                    else ChecksumDiff f storedMD5sum computedMD5sum
                                     (Left  e)              -> ChecksumError f e)
        else return $ ChecksumMissing f

checkStoredChecksumIsMissing :: FilePath -> ReaderT FilePath IO ()
checkStoredChecksumIsMissing f = do
    hasChecksum <- liftM not $ isChecksumMissing f

    unless hasChecksum (liftIO $ putStrLn $ "checksum missing: " ++ f)

checkOrphanedChecksum :: FilePath -> ReaderT FilePath IO ()
checkOrphanedChecksum md5file = do
    topdir <- ask -- e.g. /tmp/foo

    let originalFile = topdir </> reverse (drop (length (".md5sum" :: String)) $ reverse $ joinPath $ drop (1 + length (splitPath topdir)) (splitPath md5file))

    hasOriginalFile <- liftIO $ doesFileExist originalFile

    unless hasOriginalFile $ liftIO $ putStrLn $ "orphaned checksum: " ++ md5file

-- http://stackoverflow.com/questions/14259229/streaming-recursive-descent-of-a-directory-in-haskell/14261710#14261710
getRecursiveContents :: (Proxy p) => FilePath -> () -> Producer p FilePath IO ()
getRecursiveContents topPath () = runIdentityP $ do
  properNames <- fmap (filter (`notElem` [".", "..", ".md5sums"])) (lift $ getDirectoryContents topPath)
  forM_ properNames $ \name -> do
    let path = topPath </> name
    isDirectory <- lift $ doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path ()
      else respond path

-- Note on execWriterT/raiseK: http://ocharles.org.uk/blog/posts/2012-12-16-24-days-of-hackage-pipes.html
getRecursiveContentsList :: FilePath -> IO [FilePath]
getRecursiveContentsList path =
    runProxy $ execWriterK $ (getRecursiveContents path) >-> toListD

-- Read local md5sum info. Assumes that `path` ends with ".md5sum"!
readMd5Info :: FilePath -> IO (FilePath, String)
readMd5Info path = do
    when (snd (splitExtension $ last $ splitPath path) /= ".md5sum") (error $ "Path is not an .md5sum file: " ++ path)

    let baseName = reverse $ tail $ dropWhile (/= '.') (reverse path)
    mdvalue <- rstripNewline <$> S.readFile path -- need to be strict here, otherwise we get 'too many open files'
    return (baseName, mdvalue)

fixExtendedEtags :: IO ()
fixExtendedEtags = do
    sp <- s3Lines

    case sp of Right s -> do let etagPaths = map fst $ filter (\(_, y) -> '-' `elem` y) $ map (\x -> (s3Path x, s3Md5sum x)) s :: [String]
                             forM_ etagPaths (\x -> do let x_tmp = x ++ "_etag_tmp"
                                                           args1 = ["mv", x, x_tmp]
                                                           args2 = ["mv", x_tmp, x]

                                                       (Just _, Just hout1, Just herr1, _) <- createProcess (proc "s3cmd" args1){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

                                                       _       <- readRestOfHandle hout1
                                                       stderr1 <- readRestOfHandle herr1

                                                       case length stderr1 of 0 -> putStrLn $ x ++ " -> " ++ x_tmp
                                                                              _ -> error stderr1

                                                       (Just _, Just hout2, Just herr2, _) <- createProcess (proc "s3cmd" args2){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

                                                       _       <- readRestOfHandle hout2
                                                       stderr2 <- readRestOfHandle herr2

                                                       case length stderr2 of 0 -> putStrLn $ x_tmp ++ " -> " ++ x
                                                                              _ -> error stderr2)
               Left e  -> error $ show e

getLocalAndS3Report
  :: String
     -> FilePath
     -> IO (Maybe (DM.Map FilePath String, DM.Map FilePath String))
getLocalAndS3Report _s3Prefix _localMd5dir = do
    let s3Prefix = trimTrailingSlash _s3Prefix
    let path     = trimTrailingSlash _localMd5dir

    localMD5info <- DM.fromList <$> map (\(x, y) -> (trimPathPrefix path x, y)) <$> (getRecursiveContentsList path >>= mapM readMd5Info)

    sp <- s3Lines

    case sp of Right s -> return $ Just (localMD5info, DM.fromList $ map (\x -> (trimPathPrefix s3Prefix $ s3Path x, s3Md5sum x)) s)
               Left e  -> do print e
                             return Nothing

-- Report mistmatching MD5sums for files that are stored in local and S3.
reportMismatchingMd5sumsLocalVsS3 :: String -> String -> IO ()
reportMismatchingMd5sumsLocalVsS3 s3Prefix localPath = do
    let localMd5Dir = localPath </> ".md5sums"

    r <- getLocalAndS3Report s3Prefix localMd5Dir

    case r of Just (localMD5info, s3MD5info) -> do let mm = DM.toList $ DM.mapWithKey (\k _ -> mismatch k localMD5info s3MD5info) (DM.intersection localMD5info s3MD5info)
                                                   forM_ (filter (isJust . snd) mm) (\(f, msg) -> putStrLn $ f ++ " " ++ fromJust msg)
              _                              -> putStrLn "error :("

    where mismatch :: FilePath -> DM.Map FilePath String -> DM.Map FilePath String -> Maybe String
          mismatch k m1 m2 = if value1 /= value2 then Just (fromJust value1 ++ " != " ++ fromJust value2)
                                                 else Nothing
              where value1 = DM.lookup k m1
                    value2 = DM.lookup k m2

-- Report files that are stored in both S3 and the local path.
reportFilesInBoth :: String -> FilePath -> IO ()
reportFilesInBoth s3Prefix localPath = do
    let localMd5Dir = localPath </> ".md5sums"

    r <- getLocalAndS3Report s3Prefix localMd5Dir

    case r of Just (localMD5info, s3MD5info) -> forM_ (DM.toList $ DM.intersection localMD5info s3MD5info) (\x -> putStrLn $ snd x ++ " " ++ fst x)
              _                              -> putStrLn "error :("

-- Report files that are stored in local but not on S3
reportFilesInLocalButNotS3 :: String -> FilePath -> IO ()
reportFilesInLocalButNotS3 s3Prefix localPath = do
    let localMd5Dir = localPath </> ".md5sums"

    r <- getLocalAndS3Report s3Prefix localMd5Dir

    case r of Just (localMD5info, s3MD5info) -> forM_ (DM.toList $ DM.difference localMD5info s3MD5info) (\x -> putStrLn $ snd x ++ " " ++ fst x)
              _                              -> putStrLn "error :("

-- Report files that are stored in S3 not locally
reportFilesInS3ButNotLocal :: String -> FilePath -> IO ()
reportFilesInS3ButNotLocal s3Prefix localPath = do
    let localMd5Dir = localPath </> ".md5sums"

    r <- getLocalAndS3Report s3Prefix localMd5Dir

    case r of Just (localMD5info, s3MD5info) -> forM_ (DM.toList $ DM.difference s3MD5info localMD5info) (\x -> putStrLn $ snd x ++ " " ++ fst x)
              _ -> putStrLn "error :("

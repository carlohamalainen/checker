module S3Checksums ( updateS3Cache
                   , s3Lines
                   , S3Line
                   , s3Date
                   , s3Time
                   , s3Size
                   , s3Md5sum
                   , s3Path
                   ) where  

import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import qualified Data.Map as DM
import System.Directory
import System.FilePath.Posix
import System.IO
import System.Process
import Text.Parsec
import Text.Parsec.String
import Utils

data S3Line = S3Line { s3Date       :: String
                     , s3Time       :: String
                     , s3Size       :: Integer
                     , s3Md5sum     :: Either String String -- Left partial, e.g. 7dd139e41cbaba2c88b4ba4e94ccd509-21, Right normal MD5sum
                     , s3Path       :: String
                     } deriving (Show)

partialMd5sum = do
    base <- many (noneOf " -")
    char '-'
    extension <- many (noneOf " ")

    return $ base ++ "-" ++ extension

s3Line = do
    date <- many (noneOf " ")
    spaces

    time <- many (noneOf " ")
    spaces

    size <- read <$> many (noneOf " ")
    spaces

    md5 <- try (Left <$> partialMd5sum) <|> (Right <$> (many (noneOf " ")))
    spaces

    path <- many (noneOf " \n")
    newline

    return $ S3Line date time size md5 path

s3Lines = do
    x <- cacheFileName >>= readFile
    return $ parse (manyTill s3Line (newline >> eof)) "" x

cacheFileName = do
    h <- getHomeDirectory
    return $ h </> ".s3_lar"

updateS3Cache = do
    (Just hin, Just hout, Just herr, pid) <- createProcess (proc "s3cmd" ["la", "--list-md5", "--recursive"]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdout <- readRestOfHandle hout
    stderr <- readRestOfHandle herr -- FIXME handle stderr?

    f <- cacheFileName

    writeFile f stdout


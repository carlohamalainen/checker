{-# LANGUAGE RankNTypes #-}

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
                     , s3Md5sum     :: String
                     , s3Path       :: String
                     } deriving (Show)

s3Line :: forall u. ParsecT [Char] u Identity S3Line
s3Line = do
    date <- many (noneOf " ")
    spaces

    time <- many (noneOf " ")
    spaces

    size <- read <$> many (noneOf " ")
    spaces

    md5 <- many (noneOf " ")
    spaces

    path <- many (noneOf "\n")
    newline

    return $ S3Line date time size md5 path

s3Lines :: IO (Either ParseError [S3Line])
s3Lines = parse (manyTill s3Line (newline >> eof)) "" <$> (cacheFileName >>= readFile)

cacheFileName :: IO FilePath
cacheFileName = (</> ".s3_lar") <$> getHomeDirectory

updateS3Cache :: IO ()
updateS3Cache = do
    (_, Just hout, _, _) <- createProcess (proc "s3cmd" ["la", "--list-md5", "--recursive"]){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdOut <- readRestOfHandle hout
    -- stdErr <- readRestOfHandle herr -- FIXME handle stderr?

    cacheFileName >>= (flip writeFile stdOut)

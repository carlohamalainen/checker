module Utils ( readRestOfHandle
             , lstrip
             , rstrip
             , strip
             , rstripNewline
             , trimPathPrefix
             , trimTrailingSlash 
             ) where

import Control.Exception
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Proxy
import System.Directory
import System.FilePath.Posix
import System.IO
import System.Process

-- Strip spaces and tab characters.
strip  = lstrip . rstrip
lstrip = dropWhile (`elem` " \t")
rstrip = reverse . lstrip . reverse

-- Strip newline characters from the right.
rstripNewline x = reverse $ dropWhile (== '\n') (reverse x)

-- Read everything else available on a handle, and return the empty
-- string if we have hit EOF.
readRestOfHandle :: Handle -> IO String
readRestOfHandle handle = do
    ineof <- hIsEOF handle
    if ineof
        then return ""
        else do x <- hGetContents handle
                return x

trimPathPrefix prefix path = dropWhile (== '/') $ drop (length prefix) path

trimTrailingSlash path = reverse $ dropWhile (== '/') $ reverse path




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
        else hGetContents handle

-- http://www.haskell.org/pipermail/beginners/2011-April/006856.html
commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix a b = map fst (takeWhile (uncurry (==)) (zip a b))

trimPathPrefix prefix path = if [] == commonPrefix (splitPath prefix) (splitPath path)
                                then error "No common prefix: " ++ (show prefix) ++ " <-> " ++ (show path)
                                else dropWhile (== '/') $ drop (length $ trimTrailingSlash prefix) path

trimTrailingSlash path = reverse $ dropWhile (== '/') $ reverse path

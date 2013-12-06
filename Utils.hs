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
strip :: String -> String
strip  = lstrip . rstrip

lstrip :: String -> String
lstrip = dropWhile (`elem` " \t")

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

-- Strip newline characters from the right.
rstripNewline :: String -> String
rstripNewline x = reverse $ dropWhile (== '\n') (reverse x)

-- Read everything else available on a handle, and return the empty
-- string if we have hit EOF.
readRestOfHandle :: Handle -> IO String
readRestOfHandle h = do
    ineof <- hIsEOF h
    if ineof
        then return ""
        else hGetContents h

-- http://www.haskell.org/pipermail/beginners/2011-April/006856.html
commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix a b = map fst (takeWhile (uncurry (==)) (zip a b))

trimPathPrefix :: String -> String -> FilePath
trimPathPrefix prefix path = if [] == commonPrefix (splitPath prefix) (splitPath path)
                                then error "No common prefix: " ++ (show prefix) ++ " <-> " ++ (show path)
                                else dropWhile (== '/') $ drop (length $ trimTrailingSlash prefix) path

trimTrailingSlash :: String -> String
trimTrailingSlash path = reverse $ dropWhile (== '/') $ reverse path

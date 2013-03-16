module Utils ( readRestOfHandle
             , lstrip
             , rstrip
             , strip
             , rstripNewline
             , isLeft
             , isRight
             , fromLeft
             , fromRight
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

-- FIXME Surely this is in the standard library?
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- FIXME Surely this is in the standard library?
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- FIXME Surely this is in the standard library?
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Attempt to call fromRight on a Left!"

-- FIXME Surely this is in the standard library?
fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _ = error "Attempt to call fromLeft on a Right!"

trimPathPrefix prefix path = dropWhile (== '/') $ drop (length prefix) path

trimTrailingSlash path = reverse $ dropWhile (== '/') $ reverse path




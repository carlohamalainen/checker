module Main where

import Checker

import Control.Monad
import Control.Monad.Reader
import System.Directory
import System.FilePath.Posix

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.Test ()
import Test.QuickCheck.Monadic (assert, monadicIO, run, pick, PropertyM(..))

import Data.List (inits, intercalate)


import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)


arbitraryPathChar :: Gen Char
arbitraryPathChar = do
    c <- arbitrary
    if c `elem` validCharacters && c /= '/'
        then return c
        else arbitraryPathChar

arbitraryFileOrDirName :: String -> Gen String
arbitraryFileOrDirName prefix = do
    len <- choose (1, 255 - length ".md5sum") :: Gen Int
    body <- replicateM len arbitraryPathChar

    if body == "." || body == ".."
        then arbitraryFileOrDirName prefix
        else return $ prefix </> body

arbitraryPath :: String -> Gen String
arbitraryPath prefix = do
    x <- arbitraryFileOrDirName prefix

    nr <- choose (1, 5) :: Gen Int

    rest <- replicateM nr (arbitraryFileOrDirName "")

    let blah = x </> intercalate "/" rest

    if length blah < 1024 then return blah else arbitraryPath prefix

mkdirPath :: FilePath -> IO ()
mkdirPath p = do
    let dirs = map joinPath $ tail $ inits $ init $ splitDirectories p
    forM_ dirs (createDirectoryIfMissing True)

propAllChecksumsMatch :: Property
propAllChecksumsMatch = monadicIO $ do
    -- Where we do all the tests.
    topDir <- pick (arbitraryPath "/tmp/foo/")

    -- Make some files.
    nrFiles <- pick $ choose (1, 50) :: PropertyM IO Int
    files <- pick (replicateM nrFiles $ arbitraryPath topDir)
    run $ forM_ files mkdirPath

    -- Write some random content.
    contents <- pick (replicateM nrFiles arbitrary) :: PropertyM IO [String]
    run $ forM_ (zip files contents) (uncurry writeFile)

    -- Compute checksums, asserting that we get a valid
    -- md5sum for each file. This doesn't store any md5sums.
    checksums <- run $ mapM computeChecksum files
    assert $ all isRight checksums

    -- Run computeChecksums which stores md5sums in topdir/.md5sums/xxx.
    -- Assert that when checked, all md5sums match.
    run $ forM files (\f -> runReaderT (computeChecksums f) topDir)
    checksumResults <- run $ mapM (\f -> runReaderT (checkStoredChecksum f) topDir) files
    assert $ all isMatch checksumResults

    -- Remove all of the temporary files.
    run $ removeDirectoryRecursive topDir

    where

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _         = False

tests :: [TF.Test]
tests = [testGroup "QuickCheck Blahhahah" [testProperty "blah" propAllChecksumsMatch]]

main :: IO ()
main = defaultMain tests

{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for pure manipulations of a single buffer in the Vim2 Keymap.
--
-- A manipulation of a single buffer is an operation or sequence of operations
-- which do nothing other than change the contents or cursor position of a
-- single buffer.
--
-- This module loads the tests from files in @src/tests/vimtests@. Adding new
-- tests, or altering existing tests is done by editing files there. The format
-- should be self explanatory.
--
-- If a test is pure and manipulates something other than the contents or cursor
-- position of a single buffer, it should be added to the
-- 'Vim2.TestPureEditorManipulations' module.
--
module Vim2.TestPureBufferManipulations (getTests) where

import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)

import Control.Monad (filterM, forM, void, unless)

import Data.List (sort, isSuffixOf, intercalate)

import System.Directory
import System.FilePath

import Text.Printf

import Yi (extractValue)
import Yi.Buffer
import Yi.Config.Default (defaultVimConfig)
import Yi.Editor
import Yi.Keymap.Vim2

import Vim2.TestUtils

data VimTest = VimTest {
                   vtName :: String
                 , vtInput :: String
                 , vtOutput :: String
                 , vtEventString :: String
               }
    deriving (Show, Eq, Ord)

data TestResult = TestPassed String
                | TestFailed String String

instance Show TestResult where
    show (TestPassed name) = "PASSED " ++ name
    show (TestFailed name msg) = "FAILED " ++ name ++ ":\n" ++ msg

unlines' :: [String] -> String
unlines' = intercalate "\n"

loadTestFromDirectory :: FilePath -> IO VimTest
loadTestFromDirectory path = do
    [input, output, events] <- mapM (readFile' . (path </>)) ["input", "output", "events"]
    return $ VimTest (joinPath . drop 1 . splitPath $ path) input output events

isValidTestFile :: String -> Bool
isValidTestFile text =
    case lines text of
        [] -> False
        ("-- Input": ls) ->
            case break (== "-- Output") ls of
                (_, []) -> False
                (_, "-- Output":ls') -> "-- Events" `elem` ls'
                _ -> False
        _ -> False

loadTestFromFile :: FilePath -> IO VimTest
loadTestFromFile path = do
    text <- readFile' path
    unless (isValidTestFile text) $
        void $ printf "Test %s is invalid\n" path
    let ls = tail $ lines text
        (input, rest) = break (== "-- Output") ls
        (output, rest2) = break (== "-- Events") $ tail rest
        eventText = tail rest2
    return $ VimTest (joinPath . drop 1 . splitPath . dropExtension $ path)
                     (unlines' input)
                     (unlines' output)
                     (unlines' eventText)

containsTest :: FilePath -> IO Bool
containsTest d = do
    files <- fmap (filter (`notElem` [".", ".."])) $ getDirectoryContents d
    return $ sort files == ["events", "input", "output"]

getRecursiveFiles :: FilePath -> IO [FilePath]
getRecursiveFiles topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", "..", ".git", ".svn"]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveFiles path
            else return [path]
    return (concat paths)

getRecursiveDirectories :: FilePath -> IO [FilePath]
getRecursiveDirectories topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", "..", ".git", ".svn"]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then fmap (path:) $ getRecursiveDirectories path
            else return []
    return (concat paths)

discoverTests :: FilePath -> IO [VimTest]
discoverTests topdir = do
    dirs <- getRecursiveDirectories topdir
    testDirs <- filterM containsTest dirs
    testFiles <- fmap (filter (isSuffixOf ".test")) $ getRecursiveFiles topdir
    testsFromDirs <- mapM loadTestFromDirectory testDirs
    testsFromFiles <- mapM loadTestFromFile testFiles
    return $ testsFromDirs ++ testsFromFiles


mkTestCase :: VimTest -> TestTree
mkTestCase t = testCase (vtName t) $ do
    let setupActions = do
            let (cursorLine, '\n':text) = break (== '\n') (vtInput t)
            insertText text
            setCursorPosition cursorLine

        preConditions _ _ = return ()

        testActions _ = defVimEval $ vtEventString t

        assertions editor _ =
            let actualOut = cursorPos editor ++ "\n" ++
                            extractBufferString editor
            in  assertEqual (errorMsg actualOut) actualOut (vtOutput t)

    runTest setupActions preConditions testActions assertions

  where
    setCursorPosition cursorLine =
        let (x, y) = read cursorLine
        in withBuffer0 $ moveToLineColB x (y - 1)
    cursorPos = show . snd . runEditor' (withBuffer0 $ do
                                            l <- curLn
                                            c <- curCol
                                            return (l, c + 1))
    errorMsg actualOut = unlines [ "Input:", vtInput t
                                 , "Expected:", vtOutput t
                                 , "Got:", actualOut
                                 , "Events:", vtEventString t
                                 , "---"]

getTests :: IO TestTree
getTests = do
    vimtests <- discoverTests "src/tests/vimtests"
    return $ testGroup "Vim2 keymap tests" $ fmap mkTestCase . sort $ vimtests

readFile' :: FilePath -> IO String
readFile' f = do
    s <- readFile f
    return $! length s `seq` s

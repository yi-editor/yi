{-# LANGUAGE ScopedTypeVariables #-}

module TestVim (getTests) where

import Test.HUnit
import Test.Framework.Providers.HUnit
import qualified Test.Framework as TF

import Control.Monad (filterM, forM, void, unless)

import Data.List (sort, isSuffixOf, intercalate)

import System.Directory
import System.Environment
import System.FilePath

import Text.Printf

import Yi (extractValue)
import Yi.Buffer
import Yi.Config.Default (defaultVimConfig)
import Yi.Editor
import Yi.Keymap.Vim2

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

mkTestCase :: VimTest -> TF.Test
mkTestCase t = testCase (vtName t) $ assertEqual errorMsg actualOut (vtOutput t)
    where outputMatches = vtOutput t == actualOut
          actualOut = extractBufferString . fst $
              runEditor' (defVimEval $ vtEventString t)
                         (initialEditor $ vtInput t)
          extractBufferString editor = cursorPos editor ++ "\n" ++
                                       snd (runEditor' (withBuffer0 elemsB) editor)
          cursorPos = show . snd . runEditor' (withBuffer0 $ do
                                                  l <- curLn
                                                  c <- curCol
                                                  return (l, c + 1))
          errorMsg = unlines [ "Input:", vtInput t
                             , "Expected:", vtOutput t
                             , "Got:", actualOut
                             , "Events:", vtEventString t
                             , "---"]
          defVimEval = pureEval (extractValue defVimConfig)

initialEditor :: String -> Editor
initialEditor input = fst $ runEditor' action emptyEditor
    where action = withBuffer0 $ do
                       startUpdateTransactionB
                       insertN text
                       commitUpdateTransactionB
                       let (x, y) = read cursorLine
                       moveToLineColB x (y - 1)
          (cursorLine, '\n':text) = break (== '\n') input

runEditor' :: EditorM a -> Editor -> (Editor, a)
runEditor' = runEditor defaultVimConfig

getTests :: IO [TF.Test]
getTests = do
    vimtests <- discoverTests "src/tests/vimtests"
    return $! fmap mkTestCase . sort $ vimtests

readFile' :: FilePath -> IO String
readFile' f = do
    s <- readFile f
    return $! length s `seq` s
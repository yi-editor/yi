{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (filterM, forM, void, unless)

import Data.List (sort, isSuffixOf, intercalate)

import System.Directory
import System.FilePath

import Text.Printf

import Yi.Buffer.Misc
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
    [input, output, events] <- mapM (readFile . (path </>)) ["input", "output", "events"]
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
    text <- readFile path
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

discoverTests :: IO [VimTest]
discoverTests = do
    dirs <- getRecursiveDirectories "vimtests"
    testDirs <- filterM containsTest dirs
    testFiles <- fmap (filter (isSuffixOf ".test")) $ getRecursiveFiles "vimtests"
    testsFromDirs <- mapM loadTestFromDirectory testDirs
    testsFromFiles <- mapM loadTestFromFile testFiles
    return $ testsFromDirs ++ testsFromFiles

runTest :: VimTest -> TestResult
runTest t = if outputMatches then TestPassed (vtName t)
                             else TestFailed (vtName t) $
                                      unlines ["Expected:", vtOutput t, "Got:", actualOut, "---"]
    where outputMatches = vtOutput t == actualOut
          actualOut = extractBufferString $ fst $
              runEditor' (defaultVimEval $ vtEventString t) (initialEditor $ vtInput t)
          extractBufferString editor = cursorPos editor ++ "\n" ++
                                       snd (runEditor' (withBuffer0 elemsB) editor)
          cursorPos = show . snd . runEditor' (withBuffer0 $ do
                                                  l <- curLn
                                                  c <- curCol
                                                  return (l, c + 1))

initialEditor :: String -> Editor
initialEditor input = fst $ runEditor' action emptyEditor
    where action = withBuffer0 $ do
                       insertN text
                       let (x, y) = read cursorLine
                       moveToLineColB x (y - 1)
          (cursorLine, '\n':text) = break (== '\n') input

runEditor' :: EditorM a -> Editor -> (Editor, a)
runEditor' = runEditor defaultVimConfig

runTests :: [VimTest] -> ([TestResult], [TestResult])
runTests tests = (successes, failures)
    where results = map runTest tests
          successes = filter successful results
          failures = filter (not . successful) results
          successful (TestPassed _) = True
          successful _ = False

main :: IO ()
main = do
    tests <- fmap sort discoverTests

    void $ printf "Found %d tests\n\n" $ length tests

    let (successes, failures) = runTests tests

    mapM_ print failures

    let passed = length successes

    putStrLn ""
    void $ printf "PASSED: %d\n" passed
    void $ printf "FAILED: %d\n" $ length tests - passed

    return ()

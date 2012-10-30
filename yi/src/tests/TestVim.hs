module Main where

import Control.Monad (filterM, forM, void)

import Data.List (foldl', find, sort, isSuffixOf, intercalate)
import Data.Prototype (extractValue)

import System.Directory
import System.FilePath

import Text.Printf

import Yi.Buffer.Misc
import Yi.Config.Default (defaultVimConfig)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys (char)
import Yi.Keymap.Vim2
import Yi.Keymap.Vim2.Utils

data VimTest = VimTest {
                   vtName :: String
                 , vtInput :: String
                 , vtOutput :: String
                 , vtEvents :: [Event]
               }
    deriving (Show, Eq, Ord)

data TestResult = TestPassed String
                | TestFailed String String

instance Show TestResult where
    show (TestPassed name) = "PASSED " ++ name
    show (TestFailed name msg) = "FAILED " ++ name ++ ":\n" ++ msg

unlines' :: [String] -> String
unlines' = intercalate "\n"

allDefaultPureBindings :: [VimBinding]
allDefaultPureBindings = filter isPure $ allBindings $ extractValue defModeMapProto
    where isPure (VimBindingE _ _) = True
          isPure _ = False

handleEvent :: Event -> EditorM ()
handleEvent event = do
    currentState <- getDynamic
    let maybeBinding = find (isBindingApplicable event currentState) allDefaultPureBindings
    case maybeBinding of
        Nothing -> fail $ "unhandled event " ++ show event
        Just (VimBindingE _ action) -> withEditor action
        Just (VimBindingY _ _) -> fail "Impure binding found"

parseEvents :: String -> [Event]
parseEvents s = map char . filter (/= '\n') $ s

loadTestFromDirectory :: FilePath -> IO VimTest
loadTestFromDirectory path = do
    input <- readFile $ path </> "input"
    output <- readFile $ path </> "output"
    events <- fmap parseEvents $ readFile $ path </> "events"
    return $ VimTest (joinPath . drop 1 . splitPath $ path) input output events

loadTestFromFile :: FilePath -> IO VimTest
loadTestFromFile path = do
    text <- readFile path
    let ls = tail $ lines text
        (input, rest) = break (== "-- Output") ls
        (output, rest2) = break (== "-- Events") $ tail rest
        eventText = tail rest2
    return $ VimTest (joinPath . drop 1 . splitPath . dropExtension $ path)
                     (unlines' input)
                     (unlines' output)
                     (parseEvents . unlines' $ eventText)

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
          actualOut = extractBufferString $ runEvents (vtEvents t)
          extractBufferString editor = cursorPos editor ++ "\n" ++
                                       snd (runEditor' (withBuffer0 elemsB) editor)
          runEvents = foldl' runEvent $ initialEditor (vtInput t)
          runEvent editor event = fst $ runEditor' (handleEvent event) editor 
          cursorPos = show . snd . runEditor' (withBuffer0 $ do
                                                  l <- curLn
                                                  c <- curCol
                                                  return (l, c + 1))

initialEditor :: String -> Editor
initialEditor input = fst $ runEditor' action  emptyEditor
    where action = withBuffer0 $ do
                       insertN text
                       let (x, y) = read cursorLine
                       moveToLineColB x (y - 1)
          cursorLine = head $ lines input
          text = unlines' $ tail $ lines input

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

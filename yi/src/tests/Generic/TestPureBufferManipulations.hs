-- | This module aims to provide a generic back-end for other keymaps to
-- use for pure buffer manipulations. Pure buffer manipulations are considered
-- to be operations which simply change the contents of the buffer and move the
-- cursor. For example, opening a second buffer is not considered a pure buffer
-- operation.

module Generic.TestPureBufferManipulations where

import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)

import Control.Monad (filterM, forM, void, unless)

import Data.List (sort, isSuffixOf, intercalate)

import System.Directory
import System.FilePath

import Text.Printf

import Yi.Buffer
import Yi.Config (Config)
import Yi.Editor

import Generic.TestUtils

data KeymapTest = KeymapTest {
                   ktName :: String
                 , ktInput :: String
                 , ktOutput :: String
                 , ktEventString :: String
                 , ktKeysEval :: KeyEval
               }

instance Eq KeymapTest where
  KeymapTest n i o e _ == KeymapTest n' i' o' e' _ =
    n == n' && i == i' && o == o' && e == e'

instance Ord KeymapTest where
  KeymapTest n _ _ _ _ <= KeymapTest n' _ _ _ _ = n <= n'


data TestResult = TestPassed String
                | TestFailed String String

instance Show TestResult where
    show (TestPassed name) = "PASSED " ++ name
    show (TestFailed name msg) = "FAILED " ++ name ++ ":\n" ++ msg

unlines' :: [String] -> String
unlines' = intercalate "\n"

loadTestFromDirectory :: FilePath -- ^ Directory of the test
                      -> KeyEval -- ^ Function that can run
                                                -- ‘events’ commands
                      -> IO KeymapTest
loadTestFromDirectory path ev = do
    [input, output, events] <- mapM (readFile' . (path </>)) ["input", "output", "events"]
    return $ KeymapTest (joinPath . drop 1 . splitPath $ path) input output events ev

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

-- | See Arguments to 'loadTestFromDirectory'
loadTestFromFile :: FilePath -> KeyEval -> IO KeymapTest
loadTestFromFile path ev = do
    text <- readFile' path
    unless (isValidTestFile text) $
        void $ printf "Test %s is invalid\n" path
    let ls = tail $ lines text
        (input, rest) = break (== "-- Output") ls
        (output, rest2) = break (== "-- Events") $ tail rest
        eventText = tail rest2
    return $
      KeymapTest
        (joinPath . drop 1 . splitPath . dropExtension $ path)
        (unlines' input)
        (unlines' output)
        (unlines' eventText)
        ev

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

discoverTests :: FilePath -> KeyEval -> IO [KeymapTest]
discoverTests topdir ev = do
    dirs <- getRecursiveDirectories topdir
    testDirs <- filterM containsTest dirs
    testFiles <- fmap (filter (isSuffixOf ".test")) $ getRecursiveFiles topdir
    testsFromDirs <- mapM (`loadTestFromDirectory` ev) testDirs
    testsFromFiles <- mapM (`loadTestFromFile` ev) testFiles
    return $ testsFromDirs ++ testsFromFiles


mkTestCase :: Config -> KeymapTest -> TestTree
mkTestCase cf t = testCase (ktName t) $ do
    let setupActions = do
            let (cursorLine, '\n':text) = break (== '\n') (ktInput t)
            insertText text
            setCursorPosition cursorLine

        preConditions _ _ = return ()

        testActions _ = ktKeysEval t $ ktEventString t

        assertions editor _ =
            let actualOut = cursorPos editor ++ "\n" ++
                            extractBufferString cf editor
            in  assertEqual (errorMsg actualOut) actualOut (ktOutput t)

    runTest setupActions preConditions testActions assertions cf

  where
    setCursorPosition cursorLine =
        let (x, y) = read cursorLine
        in withBuffer0 $ moveToLineColB x (y - 1)
    cursorPos = show . snd . runEditor cf (withBuffer0 $ do
                                            l <- curLn
                                            c <- curCol
                                            return (l, c + 1))
    errorMsg actualOut = unlines [ "Input:", ktInput t
                                 , "Expected:", ktOutput t
                                 , "Got:", actualOut
                                 , "Events:", ktEventString t
                                 , "---"]

-- | Takes a directory with the tests, a name of the keymap
-- and an evaluation function for the keys contained in the tests.
-- For Vim2, we might do something like:
--
-- @
-- getTests defaultVimConfig "src/tests/vimtests"
--   "Vim2" (pureEval $ extractValue defVimConfig)
-- @
getTests :: Config -> FilePath -> String
         -> KeyEval -> IO TestTree
getTests c fp n ev = do
    tests <- discoverTests fp ev
    return $ testGroup (n ++ " keymap tests") $
      fmap (mkTestCase c) . sort $ tests

readFile' :: FilePath -> IO String
readFile' f = do
    s <- readFile f
    return $! length s `seq` s

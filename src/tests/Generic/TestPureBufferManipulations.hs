-- | This module aims to provide a generic back-end for other keymaps to
-- use for pure buffer manipulations. Pure buffer manipulations are considered
-- to be operations which simply change the contents of the buffer and move the
-- cursor. For example, opening a second buffer is not considered a pure buffer
-- operation.

module Generic.TestPureBufferManipulations where

import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)

import Control.Monad (filterM, forM, void, unless)
import Control.Lens ((%=))

import Data.List (sort, isSuffixOf, intercalate, isPrefixOf)
import Data.Ord (comparing)

import System.Directory
import System.FilePath

import Text.Printf

import Yi.Buffer
import Yi.Config (Config)
import Yi.Editor
import Yi.Window
import Yi.Region

import Generic.TestUtils

data KeymapTest = KeymapTest {
                   ktName :: String
                 , ktOptionalSettings :: [OptionalSetting]
                 , ktInput :: String
                 , ktOutput :: String
                 , ktEventString :: String
                 , ktKeysEval :: KeyEval
               }

data OptionalSetting = WindowSize Int Int  -- ^ WindowSize Width Height 
                     deriving Eq

instance Show OptionalSetting where
    show (WindowSize w h) = unwords ["+WindowSize", (show w), (show h)]

instance Eq KeymapTest where
  KeymapTest n s i o e _ == KeymapTest n' s' i' o' e' _ =
    n == n' && s == s' && i == i' && o == o' && e == e'

instance Ord KeymapTest where
  compare = comparing ktName


data TestResult = TestPassed String
                | TestFailed String String

instance Show TestResult where
    show (TestPassed name) = "PASSED " ++ name
    show (TestFailed name msg) = "FAILED " ++ name ++ ":\n" ++ msg

unlines' :: [String] -> String
unlines' = intercalate "\n"

optionalSettingPrefix :: String
optionalSettingPrefix = "--+ " 

isOptionalSetting :: String -> Bool
isOptionalSetting = (optionalSettingPrefix `isPrefixOf`)

decodeOptionalSetting :: [String] -> OptionalSetting
decodeOptionalSetting ["WindowSize", w, h] = WindowSize (read w) (read h)
decodeOptionalSetting unknownSetting = 
    error $ "Invalid Setting: " ++ (intercalate " " unknownSetting) 

loadTestFromDirectory :: FilePath -- ^ Directory of the test
                      -> KeyEval -- ^ Function that can run
                                                -- ‘events’ commands
                      -> IO KeymapTest
loadTestFromDirectory path ev = do
    [input, output, events] <- mapM (readFile' . (path </>)) ["input", "output", "events"]
    return $ KeymapTest (joinPath . drop 1 . splitPath $ path) [] input output events ev

isValidTestFile :: String -> Bool
isValidTestFile text =
    case (skipOptionals . lines $ text) of
        [] -> False
        ("-- Input": ls) ->
            case break (== "-- Output") ls of
                (_, []) -> False
                (_, "-- Output":ls') -> "-- Events" `elem` ls'
                _ -> False
        _ -> False
    where
        skipOptionals = dropWhile isOptionalSetting

-- | See Arguments to 'loadTestFromDirectory'
loadTestFromFile :: FilePath -> KeyEval -> IO KeymapTest
loadTestFromFile path ev = do
    text <- readFile' path
    unless (isValidTestFile text) $
        void $ printf "Test %s is invalid\n" path
    let (optionals, testContents) = span isOptionalSetting (lines text) 
        ls = tail testContents
        (input, rest) = break (== "-- Output") ls
        (output, rest2) = break (== "-- Events") $ tail rest
        eventText = tail rest2
    return $
      KeymapTest
        (joinPath . drop 1 . splitPath . dropExtension $ path)
        (map (decodeOptionalSetting . drop 1 . words) optionals)
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

optionalSettingAction :: OptionalSetting -> EditorM ()
optionalSettingAction (WindowSize w h) = 
    let region = mkSizeRegion (Point 0) (Size (w*h))
    in currentWindowA %= (\w -> w { height = h, winRegion = region })

mkTestCase :: Config -> KeymapTest -> TestTree
mkTestCase cf t = testCase (ktName t) $ do
    let setupActions = do
            let (cursorLine, '\n':text) = break (== '\n') (ktInput t)
            mapM_ optionalSettingAction $ ktOptionalSettings t
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
        in withCurrentBuffer $ moveToLineColB x (y - 1)
    cursorPos = show . snd . runEditor cf (withCurrentBuffer $ do
                                            l <- curLn
                                            c <- curCol
                                            return (l, c + 1))
    errorMsg actualOut = unlines $ optionalSettings ++
                                 [ "Input:", ktInput t
                                 , "Expected:", ktOutput t
                                 , "Got:", actualOut
                                 , "Events:", ktEventString t
                                 , "---"]
    optionalSettings = map show $ ktOptionalSettings t

-- | Takes a directory with the tests, a name of the keymap
-- and an evaluation function for the keys contained in the tests.
-- For Vim, we might do something like:
--
-- @
-- getTests defaultVimConfig "src/tests/vimtests"
--   "Vim" (pureEval $ extractValue defVimConfig)
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

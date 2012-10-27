module Main where

import Data.List (foldl', find)
import Data.Prototype (extractValue)

import System.Directory
import System.FilePath

import Yi.Buffer.Misc
import Yi.Config.Default (defaultVimConfig)
import Yi.Editor
import Yi.Event
import Yi.Keymap.Keys (char)
import Yi.Keymap.Vim2
import Yi.Keymap.Vim2.Common
import Yi.Keymap.Vim2.NormalMap
import Yi.Keymap.Vim2.InsertMap
import Yi.Keymap.Vim2.Utils

data VimTest = VimTest {
                   vtName :: String
                 , vtInput :: String
                 , vtOutput :: String
                 , vtEvents :: [Event]
               }
    deriving Show

data TestResult = TestPassed String
                | TestFailed String String

instance Show TestResult where
    show (TestPassed name) = "PASSED " ++ name
    show (TestFailed name msg) = "FAILED " ++ name ++ ":\n" ++ msg

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
        Just (VimBindingY _ action) -> fail "Impure binding found"

parseEvents :: String -> [Event]
parseEvents s = map char . filter (/= '\n') $ s

loadTest :: FilePath -> IO VimTest
loadTest path = do
    input <- readFile $ path </> "input"
    output <- readFile $ path </> "output"
    events <- fmap parseEvents $ readFile $ path </> "events"
    return $ VimTest (takeBaseName path) input output events

discoverTests :: IO [VimTest]
discoverTests = do
    testDirs <- fmap (map ("vimtests" </>) . filter (`notElem` [".", ".."])) $
                     getDirectoryContents "vimtests"
    mapM loadTest testDirs

runTest :: VimTest -> TestResult
runTest t = if outputMatches then TestPassed (vtName t)
                             else TestFailed (vtName t) $
                                      unlines [">>>", vtOutput t, "===", actualOut, "<<<"]
    where outputMatches = vtOutput t == actualOut
          actualOut = extractBufferString $ runEvents (vtEvents t)
          extractBufferString editor = cursorPos editor ++ "\n" ++
                                       snd (runEditor' (withBuffer0 elemsB) editor)
          runEvents = foldl' runEvent $ initialEditor (vtInput t)
          runEvent editor event = fst $ runEditor' (handleEvent event) editor 
          cursorPos = show . snd . runEditor' (withBuffer0 $ do
                                                  l <- curLn
                                                  c <- curCol
                                                  return (l, c))

initialEditor :: String -> Editor
initialEditor input = fst $ runEditor' action  emptyEditor
    where action = withBuffer0 $ do
                       insertN text
                       let (x, y) = read cursorLine
                       moveToLineColB y x
          cursorLine = head $ lines input
          text =  unlines $ tail $ lines input

runEditor' :: EditorM a -> Editor -> (Editor, a)
runEditor' = runEditor defaultVimConfig

main :: IO ()
main = do
    tests <- discoverTests

    putStrLn $ "Found " ++ show (length tests) ++ " tests:"

    let results = map runTest tests

    mapM_ print results

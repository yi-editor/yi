{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

-- Copyright (c) 2005,2007,2008 Jean-Philippe Bernardy

{-
  This module is aimed at being a helper for the Emacs keybindings.
  In particular this should be useful for anyone that has a custom
  keymap derived from or based on the Emacs one.
-}

module Yi.Keymap.Emacs.Utils
  ( askQuitEditor
  , askSaveEditor
  , modifiedQuitEditor
  , adjIndent
  , withMinibuffer
  , queryReplaceE
  , isearchKeymap
  , cabalConfigureE
  , cabalBuildE
  , reloadProjectE
  , executeExtendedCommandE
  , evalRegionE
  , readArgC
  , scrollDownE
  , scrollUpE
  , switchBufferE
  , killBufferE
  , insertSelf
  , insertNextC
  , findFile
  )
where

{- Standard Library Module Imports -}

import Prelude ()
import Yi.Prelude
import Data.Char
  ( ord
  , isDigit
  )
import Data.List (filter, (\\))
import Data.Maybe
  ( fromMaybe )
import System.FriendlyPath
import System.FilePath (addTrailingPathSeparator)
import System.Directory
  ( doesDirectoryExist
  )
import Control.Monad.Trans (MonadIO (..))
{- External Library Module Imports -}
{- Local (yi) module imports -}

import Control.Applicative
import Control.Monad (filterM, replicateM_)
import Control.Monad.State (gets)
import Yi.Buffer
import Yi.Buffer.HighLevel
import Yi.Buffer.Region
import Yi.Core
import Yi.Debug
import Yi.Dired
import Yi.Editor
import Yi.Eval
import Yi.File
import Yi.Keymap.Keys
import Yi.Keymap.Emacs.UnivArgument
import Yi.MiniBuffer
import Yi.Misc
import Yi.Regex
import Yi.Search
import Yi.Accessor
import Yi.Window
{- End of Module Imports -}

----------------------------
-- | Quits the editor if there are no unmodified buffers
-- if there are unmodified buffers then we ask individually for
-- each modified buffer whether or not the user wishes to save
-- it or not. If we get to the end of this list and there are still
-- some modified buffers then we ask again if the user wishes to
-- quit, but this is then a simple yes or no.
askQuitEditor, askSaveEditor :: YiM ()
askQuitEditor = askIndividualSave True =<< getModifiedBuffers

askSaveEditor = askIndividualSave False =<< getModifiedBuffers

getModifiedBuffers :: YiM [FBuffer]
getModifiedBuffers = filterM isFileBuffer =<< filter (not . isUnchangedBuffer) <$> withEditor getBuffers

-- | Is there a proper file associated with the buffer?
-- In other words, does it make sense to offer to save it?
isFileBuffer :: (Functor m, MonadIO m) => FBuffer -> m Bool
isFileBuffer b = case file b of
                   Nothing -> return False
                   Just fn -> not <$> liftIO (doesDirectoryExist fn)
                     
--------------------------------------------------
-- Takes in a list of buffers which have been identified
-- as modified since their last save.

askIndividualSave :: Bool -> [FBuffer] -> YiM ()
askIndividualSave True []  = modifiedQuitEditor
askIndividualSave False [] = return ()
askIndividualSave hasQuit allBuffers@(firstBuffer : others) =
  withEditor (spawnMinibufferE saveMessage (const askKeymap)) >> return ()
  where
  saveMessage = concat [ "do you want to save the buffer: "
                       , bufferName
                       , "? (y/n/"++ (if hasQuit then "q/" else "") ++"c/!)"
                       ]
  bufferName  = name firstBuffer

  askKeymap = choice ([ char 'n' ?>>! noAction
                      , char 'y' ?>>! yesAction 
                      , char '!' ?>>! allAction 
                      , oneOf [char 'c', ctrl $ char 'g'] >>! closeBufferAndWindowE 
                        -- cancel
                      ] ++ [char 'q' ?>>! quitEditor | hasQuit])
  yesAction = do fwriteBufferE (bkey firstBuffer)
                 withEditor closeBufferAndWindowE
                 continue

  noAction = do withEditor closeBufferAndWindowE
                continue

  allAction = do mapM_ fwriteBufferE $ fmap bkey allBuffers
                 withEditor closeBufferAndWindowE
                 askIndividualSave hasQuit []
  
  continue = askIndividualSave hasQuit others

---------------------------

---------------------------
-- | Quits the editor if there are no unmodified buffers
-- if there are then simply confirms with the user that they
-- with to quit.
modifiedQuitEditor :: YiM ()
modifiedQuitEditor =
  do modifiedBuffers <- getModifiedBuffers
     if null modifiedBuffers
        then quitEditor
        else withEditor $ spawnMinibufferE modifiedMessage (const askKeymap) >> return ()
  where
  modifiedMessage = "Modified buffers exist really quit? (y/n)"

  askKeymap = choice [ char 'n' ?>>! noAction
                     , char 'y' ?>>! quitEditor 
                     ]

  noAction        = closeBufferAndWindowE

-- | A simple wrapper to adjust the current indentation using
-- the mode specific indentation function but according to the
-- given indent behaviour.
adjIndent :: IndentBehaviour -> YiM ()
adjIndent ib = withSyntax (\m s -> modeIndent m s ib)

-----------------------------
-- isearch
selfSearchKeymap :: Keymap
selfSearchKeymap = do
  Event (KASCII c) [] <- anyEvent
  write (isearchAddE [c])

searchKeymap :: Keymap
searchKeymap = selfSearchKeymap <|> choice
               [ -- ("C-g", isearchDelE) -- Only if string is not empty.
                 ctrl (char 'r') ?>>! isearchPrevE
               , ctrl (char 's') ?>>! isearchNextE
               , ctrl (char 'w') ?>>! isearchWordE
               , meta (char 'p') ?>>! isearchHistory 1
               , meta (char 'n') ?>>! isearchHistory (-1)
               , spec KBS        ?>>! isearchDelE
               ]

isearchKeymap :: Direction -> Keymap
isearchKeymap direction = 
  do write $ isearchInitE direction
     many searchKeymap
     choice [ ctrl (char 'g') ?>>! isearchCancelE
            , oneOf [ctrl (char 'm'), spec KEnter] >>! isearchFinishE
            ] 
       <|| write isearchFinishE

----------------------------
-- query-replace
queryReplaceE :: YiM ()
queryReplaceE = do
    withMinibufferFree "Replace:" $ \replaceWhat -> do
    withMinibufferFree "With:" $ \replaceWith -> do
    b <- withEditor $ getBuffer
    win <- getA currentWindowA
    let replaceKm = choice [char 'n' ?>>! qrNext win b re,
                            char '!' ?>>! qrReplaceAll win b re replaceWith,
                            oneOf [char 'y', char ' '] >>! qrReplaceOne win b re replaceWith,
                            oneOf [char 'q', ctrl (char 'g')] >>! qrFinish
                           ]
        Just re = makeSearchOptsM [] replaceWhat
    withEditor $ do
       setRegexE re
       spawnMinibufferE
            ("Replacing " ++ replaceWhat ++ "with " ++ replaceWith ++ " (y,n,q,!):")
            (const replaceKm)
       qrNext win b re

executeExtendedCommandE :: YiM ()
executeExtendedCommandE = do
  withMinibuffer "M-x" (\_ -> getAllNamesInScope) execEditorAction

evalRegionE :: YiM ()
evalRegionE = do
  withBuffer (getSelectRegionB >>= readRegionB) >>= return -- FIXME: do something sensible.
  return ()

-- * Code for various commands
-- This ideally should be put in their own module,
-- without a prefix, so M-x ... would be easily implemented
-- by looking up that module's contents


insertSelf :: Char -> YiM ()
insertSelf = repeatingArg . insertB

-- | Insert next character, "raw"
insertNextC :: KeymapM ()
insertNextC = do c <- anyEvent
                 write $ repeatingArg $ insertB (eventToChar c)

-- | C-u stuff
readArgC :: KeymapM ()
readArgC = do readArg' Nothing
              write $ do UniversalArg u <- withEditor getDynamic
                         logPutStrLn (show u)
                         msgEditor ""

-- TODO: This is crappy code: rewrite!
readArg' :: Maybe Int -> KeymapM ()
readArg' acc = do
    write $ msgEditor $ "Argument: " ++ show acc
    c <- anyEvent -- FIXME: the C-u will read one character that should be part of the next command!
    case c of
      Event (KASCII d) [] | isDigit d -> readArg' $ Just $ 10 * (fromMaybe 0 acc) + (ord d - ord '0')
      _ -> write $ setDynamic $ UniversalArg $ Just $ fromMaybe 4 acc


-- | Open a file using the minibuffer. We have to set up some stuff to allow hints
--   and auto-completion.
findFile :: YiM ()
findFile = do maybePath <- withBuffer $ getA fileA
              startPath <- addTrailingPathSeparator <$> (liftIO $ canonicalizePath' =<< getFolder maybePath)
              -- TODO: Just call withMinibuffer
              withMinibufferGen startPath (findFileHint startPath) "find file:" (simpleComplete $ matchingFileNames (Just startPath)) $ \filename -> do
                msgEditor $ "loading " ++ filename
                fnewE filename

-- | For use as the hint when opening a file using the minibuffer.
-- We essentially return all the files in the given directory which
-- have the given prefix.
findFileHint :: String -> String -> YiM String
findFileHint startPath s = show . snd <$> getAppropriateFiles (Just startPath) s

scrollDownE :: YiM ()
scrollDownE = withUnivArg $ \a -> withBuffer $
              case a of
                 Nothing -> downScreenB
                 Just n -> replicateM_ n lineDown

scrollUpE :: YiM ()
scrollUpE = withUnivArg $ \a -> withBuffer $
              case a of
                 Nothing -> upScreenB
                 Just n -> replicateM_ n lineUp

switchBufferE :: YiM ()
switchBufferE = do
    openBufs <- fmap bufkey . toList <$> getA windowsA
    bs <- withEditor (fmap bkey <$> getBufferStack)
    let choices = (bs \\ openBufs) ++ openBufs -- put the open buffers at the end.
    names <- forM choices $ \k -> gets $ (name . findBufferWith k)
    withMinibufferFin "switch to buffer:" names (withEditor . switchToBufferWithNameE)


killBufferE :: YiM ()
killBufferE = withMinibuffer "kill buffer:" matchingBufferNames $ withEditor . closeBufferE

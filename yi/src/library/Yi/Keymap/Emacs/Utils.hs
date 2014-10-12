{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Emacs.Utils
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is aimed at being a helper for the Emacs keybindings.
-- In particular this should be useful for anyone that has a custom
-- keymap derived from or based on the Emacs one.

module Yi.Keymap.Emacs.Utils
  ( UnivArgument
  , argToInt
  , askQuitEditor
  , askSaveEditor
  , modifiedQuitEditor
  , withMinibuffer
  , queryReplaceE
  , isearchKeymap
  , cabalConfigureE
  , cabalBuildE
  , reloadProjectE
  , executeExtendedCommandE
  , evalRegionE
  , readUniversalArg
  , scrollDownE
  , scrollUpE
  , switchBufferE
  , killBufferE
  , insertNextC
  , findFile
  , findFileReadOnly
  , findFileNewTab
  , promptFile
  , promptTag
  , justOneSep
  , joinLinesE
  , countWordsRegion
  )
where

import           Control.Applicative
import           Control.Lens hiding (re,act)
import           Control.Monad
import           Control.Monad.Base
import           Data.Foldable (toList)
import           Data.List ((\\))
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import qualified Data.Text as T
import           System.Directory (doesDirectoryExist)
import           System.FilePath (takeDirectory, takeFileName, (</>))
import           System.FriendlyPath ()
import           Yi.Buffer
import           Yi.Command (cabalConfigureE, cabalBuildE, reloadProjectE)
import           Yi.Core (quitEditor)
import           Yi.Editor
import           Yi.Eval
import           Yi.File
import           Yi.Keymap
import           Yi.Keymap.Keys
import           Yi.MiniBuffer
import           Yi.Misc (promptFile)
import           Yi.Monad
import           Yi.Rectangle
import           Yi.Regex
import qualified Yi.Rope as R
import           Yi.Search
import           Yi.String
import           Yi.Tag
import           Yi.Utils
import           Yi.Window

type UnivArgument = Maybe Int

----------------------------
-- | Quits the editor if there are no unmodified buffers
-- if there are unmodified buffers then we ask individually for
-- each modified buffer whether or not the user wishes to save
-- it or not. If we get to the end of this list and there are still
-- some modified buffers then we ask again if the user wishes to
-- quit, but this is then a simple yes or no.
askQuitEditor :: YiM ()
askQuitEditor = askIndividualSave True =<< getModifiedBuffers

askSaveEditor :: YiM ()
askSaveEditor = askIndividualSave False =<< getModifiedBuffers

getModifiedBuffers :: YiM [FBuffer]
getModifiedBuffers = filterM deservesSave =<< gets bufferSet

deservesSave :: FBuffer -> YiM Bool
deservesSave b
   | isUnchangedBuffer b = return False
   | otherwise = isFileBuffer b

-- | Is there a proper file associated with the buffer?
-- In other words, does it make sense to offer to save it?
isFileBuffer :: (Functor m, MonadBase IO m) => FBuffer -> m Bool
isFileBuffer b = case b ^. identA of
  MemBuffer _ -> return False
  FileBuffer fn -> not <$> liftBase (doesDirectoryExist fn)

--------------------------------------------------
-- Takes in a list of buffers which have been identified
-- as modified since their last save.

askIndividualSave :: Bool -> [FBuffer] -> YiM ()
askIndividualSave True []  = modifiedQuitEditor
askIndividualSave False [] = return ()
askIndividualSave hasQuit allBuffers@(firstBuffer : others) =
  void (withEditor (spawnMinibufferE saveMessage (const askKeymap)))
  where
  saveMessage = T.concat [ "do you want to save the buffer: "
                         , bufferName
                         , "? (y/n/", if hasQuit then "q/" else "", "c/!)"
                         ]
  bufferName  = identString firstBuffer

  askKeymap = choice ([ char 'n' ?>>! noAction
                      , char 'y' ?>>! yesAction
                      , char '!' ?>>! allAction
                      , oneOf [char 'c', ctrl $ char 'g']
                        >>! closeBufferAndWindowE
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
        else withEditor $ void (spawnMinibufferE modifiedMessage (const askKeymap))
  where
  modifiedMessage = "Modified buffers exist really quit? (y/n)"

  askKeymap = choice [ char 'n' ?>>! noAction
                     , char 'y' ?>>! quitEditor
                     ]

  noAction        = closeBufferAndWindowE

-----------------------------
-- isearch
selfSearchKeymap :: Keymap
selfSearchKeymap = do
  Event (KASCII c) [] <- anyEvent
  write . isearchAddE $ T.singleton c

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
isearchKeymap dir =
  do write $ isearchInitE dir
     void $ many searchKeymap
     choice [ ctrl (char 'g') ?>>! isearchCancelE
            , oneOf [ctrl (char 'm'), spec KEnter]
              >>! isearchFinishWithE resetRegexE
            ]
       <|| write isearchFinishE

----------------------------
-- query-replace
queryReplaceE :: YiM ()
queryReplaceE = withMinibufferFree "Replace:" $ \replaceWhat ->
  withMinibufferFree "With:" $ \replaceWith -> do
    b <- gets currentBuffer
    win <- use currentWindowA
    let repStr = R.fromText replaceWith
        replaceKm =
          choice [ char 'n'                  ?>>! qrNext win b re
                 , char '!'                  ?>>! qrReplaceAll win b re repStr
                 , oneOf [char 'y', char ' '] >>! qrReplaceOne win b re repStr
                 , oneOf [char 'q', ctrl (char 'g')] >>! qrFinish
                 ]
        -- TODO: Yi.Regex to Text
        Right re = makeSearchOptsM [] (T.unpack replaceWhat)
        question = T.unwords [ "Replacing", replaceWhat
                             , "with", replaceWith, " (y,n,q,!):"
                             ]
    withEditor $ do
      setRegexE re
      void $ spawnMinibufferE question (const replaceKm)
      qrNext win b re


executeExtendedCommandE :: YiM ()
executeExtendedCommandE = withMinibuffer "M-x" scope act
  where
    act = execEditorAction . T.unpack
    scope = const $ map T.pack <$> getAllNamesInScope

evalRegionE :: YiM ()
evalRegionE = do
  -- FIXME: do something sensible.
  void $ withBuffer (getSelectRegionB >>= readRegionB)
  return ()

-- * Code for various commands
-- This ideally should be put in their own module,
-- without a prefix, so M-x ... would be easily implemented
-- by looking up that module's contents

-- | Insert next character, "raw"
insertNextC :: UnivArgument -> KeymapM ()
insertNextC a = do c <- anyEvent
                   write $ replicateM_ (argToInt a) $ insertB (eventToChar c)

-- | Convert the universal argument to a number of repetitions
argToInt :: UnivArgument -> Int
argToInt = fromMaybe 1


digit :: (Event -> Event) -> KeymapM Char
digit f = charOf f '0' '9'

-- TODO: replace tt by digit meta
tt :: KeymapM Char
tt = do
  Event (KASCII c) _ <- foldr1 (<|>) $ fmap (event . metaCh ) ['0'..'9']
  return c


-- doing the argument precisely is kind of tedious.
-- read: http://www.gnu.org/software/emacs/manual/html_node/Arguments.html
-- and: http://www.gnu.org/software/emacs/elisp-manual/html_node/elisp_318.html
readUniversalArg :: KeymapM (Maybe Int)
readUniversalArg = optional ((ctrlCh 'u' ?>> (read <$> some (digit id) <|> pure 4)) <|> (read <$> some tt))


-- | Finds file and runs specified action on the resulting buffer
findFileAndDo :: T.Text -- ^ Prompt
              -> (BufferRef -> YiM a) -- ^ Action to run on the resulting buffer
              -> YiM ()
findFileAndDo prompt act = promptFile prompt $ \filename -> do
  (withEditor . printMsg) $ "loading " <> filename
  void $ editFile (T.unpack filename) >>= act

-- | Open a file using the minibuffer. We have to set up some stuff to
-- allow hints and auto-completion.
findFile :: YiM ()
findFile = findFileAndDo "find file:" return

-- | Like 'findFile' but sets the resulting buffer to read-only.
findFileReadOnly :: YiM ()
findFileReadOnly = findFileAndDo "find file (read only):" $ \b ->
  withGivenBuffer b (readOnlyA .= True)

-- | Open a file in a new tab using the minibuffer.
findFileNewTab :: YiM ()
findFileNewTab = promptFile "find file (new tab): " $ \filename -> do
  withEditor newTabE
  (withEditor . printMsg) $ "loading " <> filename
  void . editFile $ T.unpack filename


scrollDownE :: UnivArgument -> BufferM ()
scrollDownE a = case a of
                 Nothing -> downScreenB
                 Just n -> scrollB n

scrollUpE :: UnivArgument -> BufferM ()
scrollUpE a = case a of
                 Nothing -> upScreenB
                 Just n -> scrollB (negate n)

-- | Prompts for a buffer name, turns it into a 'BufferRef' and passes
-- it on to the handler function. Uses all known buffers for hinting.
promptingForBuffer :: T.Text -- ^ Prompt
                   -> (BufferRef -> YiM ()) -- ^ Handler
                   -> ([BufferRef] -> [BufferRef] -> [BufferRef])
                   -- ^ Hint pre-processor. It takes the list of open
                   -- buffers and a list of all buffers, and should
                   -- spit out all the buffers to possibly hint, in
                   -- the wanted order. Note the hinter uses name
                   -- prefix for filtering regardless of what you do
                   -- here.
                   -> YiM ()
promptingForBuffer prompt act hh = do
    openBufs <- fmap bufkey . toList <$> use windowsA
    names <- withEditor $ do
      bs <- toList . fmap bkey <$> getBufferStack
      let choices = hh openBufs bs
      prefix <- gets commonNamePrefix
      forM choices $ \k ->
        gets (shortIdentString (length prefix) . findBufferWith k)
    withMinibufferFin prompt names (withEditor . getBufferWithName >=> act)

-- | Prompts the user for a buffer name and switches to the chosen buffer.
switchBufferE :: YiM ()
switchBufferE = promptingForBuffer "switch to buffer:"
                  (withEditor . switchToBufferE) (\o b -> (b \\ o) ++ o)


-- | Prompts the user for a buffer name and kills the chosen buffer.
-- Prompts about really closing if the buffer is marked as changed
-- since last save.
killBufferE :: YiM ()
killBufferE = promptingForBuffer "kill buffer:" k (\o b -> o ++ (b \\ o))
  where
    k :: BufferRef -> YiM ()
    k b = do
      buf <- withEditor . gets $ findBufferWith b
      ch <- deservesSave buf
      let askKeymap = choice [ char 'n' ?>>! closeBufferAndWindowE
                             , char 'y' ?>>! delBuf >> closeBufferAndWindowE
                             , ctrlCh 'g' ?>>! closeBufferAndWindowE
                             ]
          delBuf = deleteBuffer b
          question = identString buf <> " changed, close anyway? (y/n)"
      withEditor $
         if ch
         then void $ spawnMinibufferE question (const askKeymap)
         else delBuf


-- | If on separators (space, tab, unicode seps), reduce multiple
-- separators to just a single separator. If we aren't looking at a separator,
-- insert a single space. This kind of behaves as emacs ‘just-one-space’
-- function with the argument of ‘1’ but it prefers to use the separator we're
-- looking at instead of assuming a space.
justOneSep :: BufferM ()
justOneSep = readB >>= \c ->
  pointB >>= \point -> case point of
    Point 0 -> if isAnySep c then deleteSeparators else insertB ' '
    Point x ->
      if isAnySep c
      then deleteSeparators
      else readAtB (Point $ x - 1) >>= \d ->
        -- We weren't looking at separator but there might be one behind us
        if isAnySep d
          then moveB Character Backward >> deleteSeparators
          else insertB ' ' -- no separators, insert a space just like emacs does
  where
    deleteSeparators = do
      genMaybeMoveB unitSepThisLine (Backward, InsideBound) Backward
      moveB Character Forward
      doIfCharB isAnySep $ deleteB unitSepThisLine Forward



-- | Join this line to previous (or next N if universal)
joinLinesE :: UnivArgument -> BufferM ()
joinLinesE Nothing = return ()
joinLinesE (Just _) = do
  moveB VLine Forward
  moveToSol >> transformB (const " ") Character Backward >> justOneSep

-- | Shortcut to use a default list when a blank list is given.
-- Used for default values to emacs queries
maybeList :: [a] -> [a] -> [a]
maybeList def [] = def
maybeList _   ls = ls

maybeTag :: Tag -> T.Text -> Tag
maybeTag def t = if T.null t then def else Tag t

--------------------------------------------------
-- TAGS - See Yi.Tag for more info

-- | Prompt the user to give a tag and then jump to that tag
promptTag :: YiM ()
promptTag = do
  -- default tag is where the buffer is on
  defaultTag <- withBuffer $ Tag . R.toText <$> readUnitB unitWord
  -- if we have tags use them to generate hints
  tagTable <- withEditor getTags
  -- Hints are expensive - only lazily generate 10
  let hinter =  return . take 10 . maybe (fail . T.unpack) hintTags tagTable
  -- Completions are super-cheap. Go wild
  let completer =  return . maybe id completeTag tagTable
      p = "Find tag: (default " <> _unTag defaultTag `T.snoc` ')'
  withMinibufferGen "" hinter p completer (const $ return ()) $
    -- if the string is "" use the defaultTag
    gotoTag . maybeTag defaultTag

-- | Opens the file that contains @tag@. Uses the global tag table and prompts
-- the user to open one if it does not exist
gotoTag :: Tag -> YiM ()
gotoTag tag =
    visitTagTable $ \tagTable ->
        case lookupTag tag tagTable of
          Nothing -> fail $ "No tags containing " ++ unTag' tag
          Just (filename, line) -> do
            void $ editFile filename
            void $ withBuffer $ gotoLn line
            return ()

-- | Call continuation @act@ with the TagTable. Uses the global table
-- and prompts the user if it doesn't exist
visitTagTable :: (TagTable -> YiM ()) -> YiM ()
visitTagTable act = do
  posTagTable <- withEditor getTags
  -- does the tagtable exist?
  case posTagTable of
    Just tagTable -> act tagTable
    Nothing -> promptFile "Visit tags table: (default tags)" $ \path -> do
      -- default emacs behavior, append tags
        let p = T.unpack path
            filename = maybeList "tags" $ takeFileName p
        tagTable <- io $ importTagTable $ takeDirectory p </> filename
        withEditor $ setTags tagTable
        act tagTable

-- TODO: use TextUnit to count things inside region for better experience
-- | Counts the number of lines, words and characters inside selected
-- region. Coresponds to emacs' @count-words-region@.
countWordsRegion :: YiM ()
countWordsRegion = do
  (l, w, c) <- withEditor $ do
    t <- withBuffer0 $ getRectangle >>= \(reg, _, _) -> readRegionB reg
    let nls = R.countNewLines t
    return (if nls == 0 then 1 else nls, length $ R.words t, R.length t)
  (withEditor . printMsg) $ T.unwords [ "Region has", showT l, p l "line" <> ","
                        , showT w, p w "word" <> ", and"
                        , showT c, p w "character" <> "."
                        ]
  where
    p x w = if x == 1 then w else w <> "s"

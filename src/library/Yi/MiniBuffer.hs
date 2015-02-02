{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Minibuffer
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions working with the minibuffer.

module Yi.MiniBuffer ( spawnMinibufferE, withMinibufferFree, withMinibuffer
                     , withMinibufferGen, withMinibufferFin, noHint
                     , noPossibilities, mkCompleteFn, simpleComplete
                     , infixComplete, infixComplete', anyModeByName
                     , getAllModeNames, matchingBufferNames, anyModeByNameM
                     , anyModeName, (:::)(..), LineNumber, RegexTag
                     , FilePatternTag, ToKill, CommandArguments(..)
                     , commentRegion, promptingForBuffer, debugBufferContent
                     ) where

import           Control.Applicative            ((<$>))
import           Control.Concurrent             (threadDelay)
import           Control.Lens                   (use, (%=))
import           Control.Monad                  (forM, void, when, (<=<), (>=>))
import           Data.Foldable                  (find, toList)
import           Data.IORef                     (newIORef, readIORef, writeIORef)
import qualified Data.List.PointedList.Circular as PL (find, insertRight)
import           Data.Maybe                     (catMaybes, fromJust, fromMaybe)
import           Data.Monoid                    (mempty)
import           Data.Proxy                     (Proxy)
import           Data.String                    (IsString)
import qualified Data.Text                      as T (Text, append, head,
                                                      isInfixOf, null, pack,
                                                      snoc, unpack, words)
import           Data.Typeable                  (Typeable)
import           System.CanonicalizePath        (replaceShorthands)
import           Yi.Buffer
import           Yi.Completion
import           Yi.Config                      (modeTable)
import           Yi.Core                        (forkAction, runAction)
import           Yi.Editor
import           Yi.History                     (historyFinishGen, historyMove, historyStartGen)
import           Yi.Keymap
import           Yi.Keymap.Keys
import           Yi.Monad                       (gets)
import qualified Yi.Rope                        as R (YiString, fromText, toText)
import           Yi.String                      (commonTPrefix)
import           Yi.Style                       (defaultStyle)
import           Yi.Utils                       (io)
import           Yi.Window                      (bufkey)

-- | Prints out the rope of the current buffer as-is to stdout.
--
-- The only way to stop it is to close the buffer in question which
-- should free up the 'BufferRef'.
debugBufferContent :: YiM ()
debugBufferContent = promptingForBuffer "buffer to trace:"
                     debugBufferContentUsing (\_ x -> x)

debugBufferContentUsing :: BufferRef -> YiM ()
debugBufferContentUsing b = do
  mv <- io $ newIORef mempty
  keepGoing <- io $ newIORef True
  let delay = threadDelay 100000 >> readIORef keepGoing
  void . forkAction delay NoNeedToRefresh $
    findBuffer b >>= \case
      Nothing -> io $ writeIORef keepGoing True
      Just _ -> do
        ns <- withGivenBuffer b elemsB :: YiM R.YiString
        io $ readIORef mv >>= \c ->
          when (c /= ns) (print ns >> void (writeIORef mv ns))

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

-- | Prompts the user for comment syntax to use for the current mode.
commentRegion :: YiM ()
commentRegion =
  withCurrentBuffer (gets $ withMode0 modeToggleCommentSelection) >>= \case
    Nothing ->
      withMinibufferFree "No comment syntax is defined. Use: " $ \cString ->
        withCurrentBuffer $ do
          let toggle = toggleCommentB (R.fromText cString)
          void toggle
          modifyMode $ \x -> x { modeToggleCommentSelection = Just toggle }
    Just b -> withCurrentBuffer b

-- | Open a minibuffer window with the given prompt and keymap
-- The third argument is an action to perform after the minibuffer
-- is opened such as move to the first occurence of a searched for
-- string. If you don't need this just supply @return ()@
spawnMinibufferE :: T.Text -> KeymapEndo -> EditorM BufferRef
spawnMinibufferE prompt kmMod = do
  b <- stringToNewBuffer (MemBuffer prompt) mempty
  -- Now create the minibuffer keymap and switch to the minibuffer window
  withGivenBuffer b $
    modifyMode $ \m -> m { modeKeymap = \kms -> kms { topKeymap = kmMod (insertKeymap kms)
                                                    } }
  -- The minibuffer window must not be moved from the position newWindowE places it!
  -- First: This way the minibuffer is just below the window that was in focus when
  -- the minibuffer was spawned. This clearly indicates what window is the target of
  -- some actions. Such as searching or the :w (save) command in the Vim keymap.
  -- Second: The users of the minibuffer expect the window and buffer that was in
  -- focus when the minibuffer was spawned to be in focus when the minibuffer is closed
  -- Given that window focus works as follows:
  --    - The new window is broguht into focus.
  --    - The previous window in focus is to the left of the new window in the window
  --    set list.
  --    - When a window is deleted and is in focus then the window to the left is brought
  --    into focus.
  --
  -- If the minibuffer is moved then when the minibuffer is deleted the window brought
  -- into focus may not be the window that spawned the minibuffer.
  w <- newWindowE True b
  windowsA %= PL.insertRight w
  return b

-- | @withMinibuffer prompt completer act@: open a minibuffer with @prompt@. Once
-- a string @s@ is obtained, run @act s@. @completer@ can be used to complete
-- functions: it returns a list of possible matches.
withMinibuffer :: T.Text -> (T.Text -> YiM [T.Text]) -> (T.Text -> YiM ()) -> YiM ()
withMinibuffer prompt getPossibilities =
  withMinibufferGen "" giveHint prompt completer (const $ return ())
    where giveHint s = catMaybes . fmap (prefixMatch s) <$> getPossibilities s
          completer = simpleComplete getPossibilities

-- | Makes a completion function.
mkCompleteFn :: (T.Text -> (T.Text -> Maybe T.Text)
                 -> [T.Text] -> EditorM T.Text)
                -- ^ List completion, such as 'completeInList'.
                -> (T.Text -> T.Text -> Maybe T.Text)
                -- ^ Matcher such as 'prefixMatch'
                -> (T.Text -> YiM [T.Text])
                -- ^ Function to fetch possibilites for completion.
                -> T.Text
                -- ^ Input to try and complete against
                -> YiM T.Text
mkCompleteFn completeInListFn match getPossibilities s = do
              possibles <- getPossibilities s
              withEditor $ completeInListFn s (match s) possibles

simpleComplete :: (T.Text -> YiM [T.Text]) -> T.Text -> YiM T.Text
simpleComplete = mkCompleteFn completeInList prefixMatch

infixComplete' :: Bool -> (T.Text -> YiM [T.Text]) -> T.Text -> YiM T.Text
infixComplete' caseSensitive = mkCompleteFn completeInList' $ containsMatch' caseSensitive

infixComplete :: (T.Text -> YiM [T.Text]) -> T.Text -> YiM T.Text
infixComplete = infixComplete' True

-- | Hint function that does nothing, for use with @'withMinibufferGen'@
noHint :: a -> YiM [a]
noHint = const $ return []

noPossibilities :: String -> YiM [ String ]
noPossibilities _s = return []

-- | @withMinibufferFree prompt act@:
-- Simple version of @'withMinibufferGen'@
withMinibufferFree :: T.Text -> (T.Text -> YiM ()) -> YiM ()
withMinibufferFree prompt = withMinibufferGen "" noHint prompt
                            return (const $ return ())

-- | @withMinibufferGen proposal getHint prompt completer onTyping act@:
-- open a minibuffer with @prompt@, and initial content @proposal@. Once
-- a string @s@ is obtained, run @act s@. @completer@ can be used to
-- complete inputs by returning an incrementally better match, and
-- getHint can give an immediate feedback to the user on the current
-- input.
--
-- @on Typing@ is an extra action which will fire with every user
-- key-press and receives minibuffer contents. Use something like
-- @const $ return ()@ if you don't need this.
withMinibufferGen :: T.Text -> (T.Text -> YiM [T.Text]) -> T.Text
                  -> (T.Text -> YiM T.Text) -> (T.Text -> YiM ())
                  -> (T.Text -> YiM ()) -> YiM ()
withMinibufferGen proposal getHint prompt completer onTyping act = do
  initialBuffer <- gets currentBuffer
  initialWindow <- use currentWindowA
  let innerAction :: YiM ()
      -- ^ Read contents of current buffer (which should be the minibuffer), and
      -- apply it to the desired action
      closeMinibuffer = closeBufferAndWindowE >>
                        windowsA %= fromJust . PL.find initialWindow
      showMatchings = showMatchingsOf . R.toText =<< withCurrentBuffer elemsB
      showMatchingsOf userInput =
        printStatus =<< withDefaultStyle <$> getHint userInput
      withDefaultStyle msg = (msg, defaultStyle)
      typing = onTyping . R.toText =<< withCurrentBuffer elemsB

      innerAction = do
        lineString <- withEditor $ do
          let bufToText = R.toText <$> withCurrentBuffer elemsB
          historyFinishGen prompt bufToText
          lineString <- bufToText
          closeMinibuffer
          switchToBufferE initialBuffer
          -- The above ensures that the action is performed on the buffer
          -- that originated the minibuffer.
          return lineString
        act lineString

      up   = historyMove prompt 1
      down = historyMove prompt (-1)

      rebindings =
        choice [oneOf [spec KEnter, ctrl $ char 'm'] >>! innerAction,
                oneOf [spec KUp,    meta $ char 'p'] >>! up,
                oneOf [spec KDown,  meta $ char 'n'] >>! down,
                oneOf [spec KTab,   ctrl $ char 'i']
                  >>! completionFunction completer >>! showMatchings,
                ctrl (char 'g')                     ?>>! closeMinibuffer]

  showMatchingsOf ""
  withEditor $ do
      historyStartGen prompt
      void $ spawnMinibufferE (prompt `T.snoc` ' ')
        (\bindings -> rebindings <|| (bindings >> write showMatchings
                                      >> write typing))
      withCurrentBuffer . replaceBufferContent . R.fromText
        $ replaceShorthands proposal

-- | Open a minibuffer, given a finite number of suggestions.
withMinibufferFin :: T.Text -> [T.Text] -> (T.Text -> YiM ()) -> YiM ()
withMinibufferFin prompt possibilities act
    = withMinibufferGen "" hinter prompt completer
      (const $ return ()) (act . best)
  where
    -- The function for returning the hints provided to the user underneath
    -- the input, basically all those that currently match.
    hinter s = return $ match s
    -- All those which currently match.
    match s = filter (s `T.isInfixOf`) possibilities

    -- The best match from the list of matches
    -- If the string matches completely then we take that
    -- otherwise we just take the first match.
    best s
      | s `elem` matches = s
      | null matches       = s
      | otherwise          = head matches
      where matches = match s

    -- We still want "TAB" to complete even though the user could just
    -- press return with an incomplete possibility. The reason is we
    -- may have for example two possibilities which share a long
    -- prefix and hence we wish to press tab to complete up to the
    -- point at which they differ.
    completer s = return $ fromMaybe s $ commonTPrefix $ catMaybes (infixMatch s <$> possibilities)

-- | TODO: decide whether we should be keeping 'T.Text' here or moving
-- to 'YiString'.
completionFunction :: (T.Text -> YiM T.Text) -> YiM ()
completionFunction f = do
  p <- withCurrentBuffer pointB
  let r = mkRegion 0 p
  text <- withCurrentBuffer $ readRegionB r
  compl <- R.fromText <$> f (R.toText text)

  -- it's important to do this before removing the text, so if the
  -- completion function raises an exception, we don't delete the
  -- buffer contents.
  withCurrentBuffer $ replaceRegionB r compl

class Promptable a where
    getPromptedValue :: T.Text -> YiM a
    getPrompt :: Proxy a -> T.Text
    getMinibuffer :: Proxy a -> T.Text -> (T.Text -> YiM ()) -> YiM ()
    getMinibuffer _ = withMinibufferFree

doPrompt :: forall a. Promptable a => (a -> YiM ()) -> YiM ()
doPrompt act = getMinibuffer witness (getPrompt witness `T.append` ":") (act <=< getPromptedValue)
  where
    witness = undefined
    witness :: Proxy a

instance Promptable String where
    getPromptedValue = return . T.unpack
    getPrompt _ = "String"

instance Promptable Char where
    getPromptedValue x = if T.null x
                         then error "Please supply a character."
                         else return $ T.head x
    getPrompt _ = "Char"

instance Promptable Int where
    getPromptedValue = return . read . T.unpack
    getPrompt _ = "Integer"

instance Promptable T.Text where
  getPromptedValue = return
  getPrompt _ = "Text"

instance Promptable R.YiString where
  getPromptedValue = return . R.fromText
  getPrompt _ = "YiString"

-- helper functions:
getPromptedValueList :: [(T.Text, a)] -> T.Text -> YiM a
getPromptedValueList vs s = maybe (error "Invalid choice") return (lookup s vs)

getMinibufferList :: [(T.Text, a)] -> Proxy a -> T.Text
                  -> (T.Text -> YiM ()) -> YiM ()
getMinibufferList vs _ prompt = withMinibufferFin prompt (fmap fst vs)

enumAll :: (Enum a, Bounded a, Show a) => [(T.Text, a)]
enumAll = fmap (\v -> (T.pack $ show v, v)) [minBound..]

instance Promptable Direction where
    getPromptedValue = getPromptedValueList enumAll
    getPrompt _ = "Direction"
    getMinibuffer = getMinibufferList enumAll

textUnits :: [(T.Text, TextUnit)]
textUnits =
       [("Character", Character),
        ("Document", Document),
        ("Line", Line),
        ("Paragraph", unitParagraph),
        ("Word", unitWord),
        ("ViWord", unitViWord)
       ]

instance Promptable TextUnit where
    getPromptedValue = getPromptedValueList textUnits
    getPrompt _ = "Unit"
    getMinibuffer = getMinibufferList textUnits

instance Promptable Point where
    getPromptedValue s = Point <$> getPromptedValue s
    getPrompt _ = "Point"

anyModeName :: AnyMode -> T.Text
anyModeName (AnyMode m) = modeName m

-- TODO: Better name
anyModeByNameM :: T.Text -> YiM (Maybe AnyMode)
anyModeByNameM n = find ((n==) . anyModeName) . modeTable <$> askCfg

anyModeByName :: T.Text -> YiM AnyMode
anyModeByName n = anyModeByNameM n >>= \case
  Nothing -> fail $ "anyModeByName: no such mode: " ++ T.unpack n
  Just m  -> return m

getAllModeNames :: YiM [T.Text]
getAllModeNames = fmap anyModeName . modeTable <$> askCfg

instance Promptable AnyMode where
    getPrompt _ = "Mode"
    getPromptedValue = anyModeByName
    getMinibuffer _ prompt act = do
      names <- getAllModeNames
      withMinibufferFin prompt names act

instance Promptable BufferRef where
    getPrompt _ = "Buffer"
    getPromptedValue = getBufferWithNameOrCurrent
    getMinibuffer _ prompt act = do
      bufs <- matchingBufferNames
      withMinibufferFin prompt bufs act

-- | Returns all the buffer names
matchingBufferNames :: YiM [T.Text]
matchingBufferNames = withEditor $ do
  p <- gets commonNamePrefix
  bs <- gets bufferSet
  return $ fmap (shortIdentString $ length p) bs

instance (YiAction a x, Promptable r) => YiAction (r -> a) x where
    makeAction f = YiA $ doPrompt (runAction . makeAction . f)

-- | Tag a type with a documentation
newtype (:::) t doc = Doc {fromDoc :: t} deriving (Eq, Typeable, Num, IsString)

instance Show x => Show (x ::: t) where
    show (Doc d) = show d

instance (DocType doc, Promptable t) => Promptable (t ::: doc) where
    getPrompt _ = typeGetPrompt (error "typeGetPrompt should not enter its argument" :: doc)
    getPromptedValue x = Doc <$> getPromptedValue x

class DocType t where
    -- | What to prompt the user when asked this type?
    typeGetPrompt :: t -> T.Text

data LineNumber
instance DocType LineNumber where
    typeGetPrompt _ = "Line"

data ToKill
instance DocType ToKill where
    typeGetPrompt _ = "kill buffer"


data RegexTag deriving Typeable
instance DocType RegexTag where
    typeGetPrompt _ = "Regex"

data FilePatternTag deriving Typeable
instance DocType FilePatternTag where
    typeGetPrompt _ = "File pattern"

newtype CommandArguments = CommandArguments [T.Text]
    deriving (Show, Eq, Typeable)

instance Promptable CommandArguments where
    getPromptedValue = return . CommandArguments . T.words
    getPrompt _ = "Command arguments"

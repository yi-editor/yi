{-# LANGUAGE
  ScopedTypeVariables,
  FlexibleInstances,
  MultiParamTypeClasses,
  UndecidableInstances,
  TypeSynonymInstances,
  TypeOperators,
  EmptyDataDecls,
  DeriveDataTypeable,
  GeneralizedNewtypeDeriving,
  OverloadedStrings #-}

module Yi.MiniBuffer
 (
  spawnMinibufferE,
  withMinibufferFree, withMinibuffer, withMinibufferGen, withMinibufferFin,
  noHint, noPossibilities, mkCompleteFn, simpleComplete, infixComplete, infixComplete', anyModeByName, getAllModeNames,
  matchingBufferNames, anyModeByNameM, anyModeName,

  (:::)(..),
  LineNumber, RegexTag, FilePatternTag, ToKill,
  CommandArguments(..)
 ) where

import Control.Applicative
import Control.Monad
import Control.Lens hiding (act)
import Data.List (isInfixOf)
import Data.Proxy
import qualified Data.List.PointedList.Circular as PL
import Data.Maybe
import Data.String (IsString)
import Data.Typeable
import Data.Foldable (find)
import Yi.Config
import Yi.Core
import Yi.History
import Yi.Completion (infixMatch, prefixMatch, containsMatch', completeInList, completeInList')
import Yi.Style (defaultStyle)
import Yi.Utils
import Yi.Monad
import System.CanonicalizePath (replaceShorthands)

-- | Open a minibuffer window with the given prompt and keymap
-- The third argument is an action to perform after the minibuffer
-- is opened such as move to the first occurence of a searched for
-- string. If you don't need this just supply @return ()@
spawnMinibufferE :: String -> KeymapEndo -> EditorM BufferRef
spawnMinibufferE prompt kmMod =
    do b <- stringToNewBuffer (Left prompt) ""
       -- Now create the minibuffer keymap and switch to the minibuffer window
       withGivenBuffer0 b $
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
       (%=) windowsA (PL.insertRight w)
       return b

-- | @withMinibuffer prompt completer act@: open a minibuffer with @prompt@. Once
-- a string @s@ is obtained, run @act s@. @completer@ can be used to complete
-- functions: it returns a list of possible matches.
withMinibuffer :: String -> (String -> YiM [String]) -> (String -> YiM ()) -> YiM ()
withMinibuffer prompt getPossibilities =
  withMinibufferGen "" giveHint prompt completer (const $ return ())
    where giveHint s = catMaybes . fmap (prefixMatch s) <$> getPossibilities s
          completer = simpleComplete getPossibilities

mkCompleteFn :: (String -> (String -> Maybe String) -> [String] -> EditorM String) ->
                (String -> String -> Maybe String) -> (String -> YiM [String]) -> String -> YiM String
mkCompleteFn completeInListFn match getPossibilities s = do
              possibles <- getPossibilities s
              withEditor $ completeInListFn s (match s) possibles

simpleComplete :: (String -> YiM [String]) -> String -> YiM String
simpleComplete = mkCompleteFn completeInList prefixMatch

infixComplete' :: Bool -> (String -> YiM [String]) -> String -> YiM String
infixComplete' caseSensitive = mkCompleteFn completeInList' $ containsMatch' caseSensitive

infixComplete :: (String -> YiM [String]) -> String -> YiM String
infixComplete = infixComplete' True

-- | Hint function that does nothing, for use with @'withMinibufferGen'@
noHint :: String -> YiM [String]
noHint = const $ return []

noPossibilities :: String -> YiM [ String ]
noPossibilities _s = return []

-- | @withMinibufferFree prompt act@:
-- Simple version of @'withMinibufferGen'@
withMinibufferFree :: String -> (String -> YiM ()) -> YiM ()
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
withMinibufferGen :: String -> (String -> YiM [String]) -> String
                  -> (String -> YiM String) -> (String -> YiM ())
                  -> (String -> YiM ()) -> YiM ()
withMinibufferGen proposal getHint prompt completer onTyping act = do
  initialBuffer <- gets currentBuffer
  initialWindow <- use currentWindowA
  let innerAction :: YiM ()
      -- ^ Read contents of current buffer (which should be the minibuffer), and
      -- apply it to the desired action
      closeMinibuffer = closeBufferAndWindowE >>
                        (%=) windowsA (fromJust . PL.find initialWindow)
      showMatchings = showMatchingsOf =<< withBuffer elemsB
      showMatchingsOf userInput =
        withEditor . printStatus =<< fmap withDefaultStyle (getHint userInput)
      withDefaultStyle msg = (msg, defaultStyle)
      -- typing = withEditor . onTyping =<< withBuffer elemsB
      typing = onTyping =<< withBuffer elemsB

      innerAction = do
        lineString <- withEditor $ do
          historyFinishGen prompt (withBuffer0 elemsB)
          lineString <- withBuffer0 elemsB
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
      void $ spawnMinibufferE (prompt ++ " ")
        (\bindings -> rebindings <|| (bindings >> write showMatchings
                                      >> write typing))
      withBuffer0 $ replaceBufferContent (replaceShorthands proposal)

-- | Open a minibuffer, given a finite number of suggestions.
withMinibufferFin :: String -> [String] -> (String -> YiM ()) -> YiM ()
withMinibufferFin prompt possibilities act
    = withMinibufferGen "" hinter prompt completer
      (const $ return ()) (act . best)
      where
        -- The function for returning the hints provided to the user underneath
        -- the input, basically all those that currently match.
        hinter s = return $ match s
        -- All those which currently match.
        match s = filter (s `isInfixOf`) possibilities

        -- The best match from the list of matches
        -- If the string matches completely then we take that
        -- otherwise we just take the first match.
        best s
          | s `elem` matches = s
          | null matches       = s
          | otherwise          = head matches
          where matches = match s
        -- We still want "TAB" to complete even though the user could just press
        -- return with an incomplete possibility. The reason is we may have for
        -- example two possibilities which share a long prefix and hence we wish
        -- to press tab to complete up to the point at which they differ.
        completer s = return $ case commonPrefix $ catMaybes $ fmap (infixMatch s) possibilities of
            "" -> s
            p -> p

completionFunction :: (String -> YiM String) -> YiM ()
completionFunction f = do
  p <- withBuffer pointB
  let r = mkRegion 0 p
  text <- withBuffer $ readRegionB r
  compl <- f text
  -- it's important to do this before removing the text,
  -- so if the completion function raises an exception, we don't delete the buffer contents.
  withBuffer $ replaceRegionB r compl

class Promptable a where
    getPromptedValue :: String -> YiM a
    getPrompt :: Proxy a -> String
    getMinibuffer :: Proxy a -> String -> (String -> YiM ()) -> YiM ()
    getMinibuffer _ = withMinibufferFree

doPrompt :: forall a. Promptable a => (a -> YiM ()) -> YiM ()
doPrompt act = getMinibuffer witness (getPrompt witness ++ ":") (act <=< getPromptedValue)
    where witness = undefined
          witness :: Proxy a

instance Promptable String where
    getPromptedValue = return
    getPrompt _ = "String"

instance Promptable Char where
    getPromptedValue x = if null x then error "Please supply a character."
                         else return $ head x
    getPrompt _ = "Char"

instance Promptable Int where
    getPromptedValue = return . read
    getPrompt _ = "Integer"

-- helper functions:
getPromptedValueList :: [(String,a)] -> String -> YiM a
getPromptedValueList vs s = maybe (error "Invalid choice") return (lookup s vs)

getMinibufferList :: [(String,a)] -> Proxy a -> String -> (String -> YiM ()) -> YiM ()
getMinibufferList vs _ prompt = withMinibufferFin prompt (fmap fst vs)

enumAll :: (Enum a, Bounded a, Show a) => [(String, a)]
enumAll = fmap (\v -> (show v, v)) [minBound..]

instance Promptable Direction where
    getPromptedValue = getPromptedValueList enumAll
    getPrompt _ = "Direction"
    getMinibuffer = getMinibufferList enumAll

textUnits :: [(String, TextUnit)]
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

anyModeName :: AnyMode -> String
anyModeName (AnyMode m) = modeName m

-- TODO: Better name
anyModeByNameM :: String -> YiM (Maybe AnyMode)
anyModeByNameM n = find ((n==) . anyModeName) . modeTable <$> askCfg

anyModeByName :: String -> YiM AnyMode
anyModeByName n = maybe (fail "no such mode") return =<< anyModeByNameM n

getAllModeNames :: YiM [String]
getAllModeNames = fmap anyModeName . modeTable <$> askCfg

instance Promptable AnyMode where
    getPrompt _ = "Mode"
    getPromptedValue = anyModeByName
    getMinibuffer _ prompt act = do
      names <- getAllModeNames
      withMinibufferFin prompt names act

instance Promptable BufferRef where
    getPrompt _ = "Buffer"
    getPromptedValue = withEditor . getBufferWithNameOrCurrent
    getMinibuffer _ prompt act = do
      bufs <- matchingBufferNames ""
      withMinibufferFin prompt bufs act

-- | Returns all the buffer names.
matchingBufferNames :: String -> YiM [String]
matchingBufferNames _ = withEditor $ do
  p <- gets commonNamePrefix
  bs <- gets bufferSet
  return $ fmap (shortIdentString p) bs


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
    typeGetPrompt :: t -> String

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

newtype CommandArguments = CommandArguments [String]
    deriving Typeable

instance Promptable CommandArguments where
    getPromptedValue = return . CommandArguments . words
    getPrompt _ = "Command arguments"

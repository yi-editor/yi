{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeSynonymInstances, 
  TypeOperators, EmptyDataDecls, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

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

import Prelude (filter, length, words)
import Data.List (isInfixOf, sortBy)
import qualified Data.List.PointedList.Circular as PL
import Data.Maybe
import Data.String (IsString)
import System.FilePath (takeDirectory)
import Yi.Config
import Yi.Core
import Yi.History
import Yi.Completion (infixMatch, prefixMatch, containsMatch', completeInList, completeInList')
import Yi.Style (defaultStyle, withFg, green)
import Shim.Utils (fuzzyDistance)
import qualified Data.Rope as R

-- | Open a minibuffer window with the given prompt and keymap
-- The third argument is an action to perform after the minibuffer
-- is opened such as move to the first occurence of a searched for
-- string. If you don't need this just supply @return ()@
spawnMinibufferE :: String -> KeymapEndo -> EditorM BufferRef
spawnMinibufferE prompt kmMod =
    do b <- stringToNewBuffer (Left prompt) (R.fromString "")
       -- Now create the minibuffer keymap and switch to the minibuffer window
       withGivenBuffer0 b $ do
         modifyMode $ \m -> m { modeKeymap = \kms -> kms { topKeymap = kmMod (insertKeymap kms)
                                                         , startTopKeymap = kmMod (startInsertKeymap kms)
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
       modA windowsA (PL.insertRight w)
       return b

-- | @withMinibuffer prompt completer act@: open a minibuffer with @prompt@. Once
-- a string @s@ is obtained, run @act s@. @completer@ can be used to complete
-- functions: it returns a list of possible matches.
withMinibuffer :: String -> (String -> YiM [String]) -> (String -> YiM ()) -> YiM ()
withMinibuffer prompt getPossibilities act = 
  withMinibufferGen "" giveHint prompt completer act
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

noHint :: String -> YiM [String]
noHint = const $ return []

noPossibilities :: String -> YiM [ String ]
noPossibilities _s = return []

withMinibufferFree :: String -> (String -> YiM ()) -> YiM ()
withMinibufferFree prompt = withMinibufferGen "" noHint prompt return

-- | @withMinibufferGen proposal getHint prompt completer act@: open a minibuffer
-- with @prompt@, and initial content @proposal@. Once a string @s@ is obtained,
-- run @act s@. @completer@ can be used to complete inputs by returning an
-- incrementally better match, and getHint can give an immediate feedback to the
-- user on the current input.
withMinibufferGen :: String -> (String -> YiM [String]) -> 
                     String -> (String -> YiM String) -> (String -> YiM ()) -> YiM ()
withMinibufferGen proposal getHint prompt completer act = do
  initialBuffer <- gets currentBuffer
  initialWindow <- getA currentWindowA
  let realDo :: YiM ()
      smartDo :: YiM ()
      -- ^ Read contents of current buffer (which should be the minibuffer), and
      -- apply it to the desired action
      closeMinibuffer = closeBufferAndWindowE >>
                        modA windowsA (fromJust . PL.find initialWindow)
      showMatchings = showMatchingsOf =<< withBuffer elemsB
      
      -- add { | } to the status line, make it looks like ido-mode in emacs
      safeTail [] = []
      safeTail xs = tail xs
      
      transform = (++ ["}"]) . ("{" :) . safeTail . foldr (\x acc -> ["|", x] ++ acc) []
      showMatchingsOf userInput = withEditor . printStatus =<< fmap (withHintStyle . transform) (getHint userInput)
      
      hintStyle = const $ withFg green
      withHintStyle msg = (msg, hintStyle)
      

      getLineString = withEditor $ do historyFinishGen prompt (withBuffer0 elemsB)
                                      lineString <- withBuffer0 elemsB
                                      closeMinibuffer
                                      switchToBufferE initialBuffer
                                      -- The above ensures that the action is performed on the buffer
                                      -- that originated the minibuffer.
                                      return lineString
      realDo = getLineString >>= act
        
      smartDo = do lineString <- withEditor $ withBuffer0 elemsB
                   -- if is directory, continue, else open the file
                   if not (null lineString) && last lineString == '/' then showMatchings
                     else realDo

      -- todo: this is find-file spec
      gotoParentDir :: YiM ()
      gotoParentDir = do
        lineString <- withEditor $ withBuffer0 elemsB
        toParent lineString
        -- goto the parent's dir
        where toParent []                   = error "path is nil."
              toParent (x:[])               = error "Get to the root directory."
              toParent xs | last xs == '/'  = replaceBuffer $ takeDirectory $ init xs
                          | otherwise       = replaceBuffer $ takeDirectory xs

              replaceBuffer = withEditor . withBuffer0 . replaceBufferContent . (++ "/")

      -- delete one char
      deleteChar :: YiM()
      deleteChar = do
        lineString <- withEditor $ withBuffer0 elemsB
        if last lineString == '/' then gotoParentDir
          else withEditor $ withBuffer0 $ replaceBufferContent $ init lineString
                         
      up   = historyMove prompt 1
      down = historyMove prompt (-1)

      rebindings = choice [ctrl (char 'j')    		       ?>>! realDo,
                           oneOf [spec KEnter, ctrl $ char 'm'] >>! completionFunction completer >>! smartDo,
                           ctrl (char 's') 		       ?>>! realDo, -- to-do: change the matchings order
                           ctrl (char 'r') 		       ?>>! realDo, -- to-do: change the matchings order
                           oneOf [ctrl (char 'w')]              >>! gotoParentDir >>! showMatchings,
                           oneOf [spec KBS]                     >>! deleteChar >>! showMatchings,
--                           oneOf [spec KTab,   ctrl $ char 'i'] >>! completionFunction completer >>! showMatchings,
                           oneOf [spec KTab,   ctrl $ char 'i'] >>! realDo,
                           ctrl (char 'g')                     ?>>! closeMinibuffer]
  logPutStrLn "show matching of"
  showMatchingsOf ""
  logPutStrLn "show proposal"
  withEditor $ do 
      historyStartGen prompt
      discard $ spawnMinibufferE (prompt ++ " ") (\bindings -> rebindings <|| (bindings >> write showMatchings))
      withBuffer0 $ replaceBufferContent proposal

-- | Open a minibuffer, given a finite number of suggestions.
withMinibufferFin :: String -> [String] -> (String -> YiM ()) -> YiM ()
withMinibufferFin prompt possibilities act 
    = withMinibufferGen "" hinter prompt completer (act . best)
      where 
        -- The function for returning the hints provided to the user underneath
        -- the input, basically all those that currently match.
        hinter s = return $ match s
        -- just sort
        match s = sortBy (compare `on` (fuzzyDistance s)) possibilities
--        match s = filter (s `isInfixOf`) possibilities

        -- The best match from the list of matches
        -- If the string matches completely then we take that
        -- otherwise we just take the first match.
        best s
          | any (== s) matches = s
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
    getPrompt :: a -> String           -- Parameter can be "undefined/bottom"
    getMinibuffer :: a -> String -> (String -> YiM ()) -> YiM ()
    getMinibuffer _ = withMinibufferFree

doPrompt :: forall a. Promptable a => (a -> YiM ()) -> YiM ()
doPrompt act = getMinibuffer witness (getPrompt witness ++ ":") $ 
                     \string -> act =<< getPromptedValue string
    where witness = error "Promptable argument should not be accessed"
          witness :: a

instance Promptable String where
    getPromptedValue = return
    getPrompt _ = "String"

instance Promptable Char where
    getPromptedValue x = if length x == 0 then error "Please supply a character." 
                         else return $ head x
    getPrompt _ = "Char"

instance Promptable Int where
    getPromptedValue = return . read
    getPrompt _ = "Integer"

-- helper functions:
getPromptedValueList :: [(String,a)] -> String -> YiM a
getPromptedValueList vs s = maybe (error "Invalid choice") return (lookup s vs)

getMinibufferList :: [(String,a)] -> a -> String -> (String -> YiM ()) -> YiM ()
getMinibufferList vs _ prompt act = withMinibufferFin prompt (fmap fst vs) act

enumAll :: (Enum a, Bounded a, Show a) => [(String, a)]
enumAll = (fmap (\v -> (show v, v)) [minBound..])

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
    



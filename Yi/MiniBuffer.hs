{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeSynonymInstances, 
  TypeOperators, EmptyDataDecls, DeriveDataTypeable #-}

module Yi.MiniBuffer 
 (
  spawnMinibufferE,
  withMinibufferFree, withMinibuffer, withMinibufferGen, withMinibufferFin, 
  noHint, noPossibilities, simpleComplete, anyModeByName, getAllModeNames,
  matchingBufferNames, anyModeByNameM, anyModeName,

  (:::)(..),
  LineNumber, RegexTag, FilePatternTag
 ) where

import Prelude (filter, length)
import Data.List (isInfixOf)
import qualified Data.List.PointedList.Circular as PL
import Data.Maybe
import Yi.Config
import Yi.Core
import Yi.History
import Yi.Completion (commonPrefix, infixMatch, prefixMatch, completeInList)
import qualified Yi.Core as Editor
import Control.Monad.Reader
import qualified Data.Accessor.MonadState as AM
-- | Open a minibuffer window with the given prompt and keymap
-- The third argument is an action to perform after the minibuffer
-- is opened such as move to the first occurence of a searched for
-- string. If you don't need this just supply @return ()@
spawnMinibufferE :: String -> KeymapEndo -> EditorM BufferRef
spawnMinibufferE prompt kmMod =
    do b <- stringToNewBuffer (Left prompt) (fromString "")
       withGivenBuffer0 b $ modifyMode (\m -> m {modeKeymap = kmMod})
       w <- newWindowE True b
       modA windowsA (PL.insertRight w)
       return b

-- | @withMinibuffer prompt completer act@: open a minibuffer with @prompt@. Once
-- a string @s@ is obtained, run @act s@. @completer@ can be used to complete
-- functions: it returns a list of possible matches.
withMinibuffer :: String -> (String -> YiM [String]) -> (String -> YiM ()) -> YiM ()
withMinibuffer prompt getPossibilities act = 
  withMinibufferGen "" giveHint prompt completer act
    where giveHint s = show . catMaybes . fmap (prefixMatch s) <$> getPossibilities s
          completer = simpleComplete getPossibilities

simpleComplete :: (String -> YiM [String]) -> String -> YiM String
simpleComplete getPossibilities s = do
              possibles <- getPossibilities s
              withEditor $ completeInList s (prefixMatch s) possibles

noHint :: String -> YiM String
noHint = const $ return ""

noPossibilities :: String -> YiM [ String ]
noPossibilities _s = return []

withMinibufferFree :: String
                                                  -> (String -> YiM ())
                                                  -> YiM ()
withMinibufferFree prompt = withMinibufferGen "" noHint prompt return

-- | @withMinibuffer proposal getHint prompt completer act@: open a minibuffer
-- with @prompt@, and initial content @proposal@. Once a string @s@ is obtained,
-- run @act s@. @completer@ can be used to complete inputs by returning an
-- incrementally better match, and getHint can give an immediate feedback to the
-- user on the current input.
withMinibufferGen :: String -> (String -> YiM String) -> 
                     String -> (String -> YiM String) -> (String -> YiM ()) -> YiM ()
withMinibufferGen proposal getHint prompt completer act = do
  initialBuffer <- gets currentBuffer
  initialWindow <- AM.get currentWindowA
  let innerAction :: YiM ()
      -- ^ Read contents of current buffer (which should be the minibuffer), and
      -- apply it to the desired action
      closeMinibuffer = closeBufferAndWindowE >>
                        modA windowsA (fromJust . PL.find initialWindow)
      showMatchings = msgEditor =<< getHint =<< withBuffer elemsB
      innerAction = do
        lineString <- withEditor $ do historyFinishGen prompt (withBuffer0 elemsB)
                                      lineString <- withBuffer0 elemsB
                                      closeMinibuffer
                                      switchToBufferE initialBuffer
                                      -- The above ensures that the action is performed on the buffer
                                      -- that originated the minibuffer.
                                      return lineString
        act lineString
      up   = historyMove prompt 1
      down = historyMove prompt (-1)

      rebindings = choice [oneOf [spec KEnter, ctrl $ char 'm'] >>! innerAction,
                           oneOf [spec KUp,    meta $ char 'p'] >>! up,
                           oneOf [spec KDown,  meta $ char 'n'] >>! down,
                           oneOf [spec KTab,   ctrl $ char 'i'] >>! completionFunction completer >>! showMatchings,
                           ctrl (char 'g')                     ?>>! closeMinibuffer]
  msgEditor =<< getHint ""
  withEditor $ do 
      historyStartGen prompt
      spawnMinibufferE (prompt ++ " ") (\bindings -> rebindings <|| (bindings >> write showMatchings))
      withBuffer0 $ replaceBufferContent proposal


-- | Open a minibuffer, given a finite number of suggestions.
withMinibufferFin :: String -> [String] -> (String -> YiM ()) -> YiM ()
withMinibufferFin prompt posibilities act 
    = withMinibufferGen "" hinter prompt completer (act . best)
      where 
        -- The function for returning the hints provided to the user underneath
        -- the input, basically all those that currently match.
        hinter s = return $ show $ match s
        -- All those which currently match.
        match s = filter (s `isInfixOf`) posibilities

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
        completer s = return $ case commonPrefix $ catMaybes $ fmap (infixMatch s) posibilities of
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
    getPromptedValue _ = error "Promptable: getPromptedValue missing"
    getPrompt :: a -> String           -- Parameter can be "undefined"
    getPrompt _ = "Promptable: prompt missing"
    doPrompt :: (a -> YiM ()) -> YiM ()
    doPrompt act = withMinibufferFree (getPrompt (undefined::a) ++ ":") $ 
                     \string -> act =<< getPromptedValue string

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
    doPrompt act = do
      names <- getAllModeNames
      withMinibufferFin "Mode" names $ anyModeByName >=> act

instance Promptable BufferRef where
    doPrompt act = do 
      bufs <- matchingBufferNames ""
      withMinibufferFin "Buffer" bufs $ \n -> 
          do b <- withEditor $ getBufferWithName n
             act b

-- | Returns all the buffer names.
matchingBufferNames :: String -> YiM [String]
matchingBufferNames _ = withEditor $ do
  p <- gets commonNamePrefix 
  bs <- gets bufferSet
  return $ fmap (shortIdentString p) bs


instance (YiAction a x, Promptable r) => YiAction (r -> a) x where
    makeAction f = YiA $ doPrompt (runAction . makeAction . f)
                   

-- | Tag a type with a documentation
newtype (:::) t doc = Doc {fromDoc :: t} deriving Typeable

instance (DocType doc, Promptable t) => Promptable (t ::: doc) where
    getPrompt _ = typeGetPrompt (error "typeGetPrompt should not enter its argument" :: doc)
    getPromptedValue x = Doc <$> getPromptedValue x

class DocType t where
    -- | What to prompt the user when asked this type?
    typeGetPrompt :: t -> String 

data LineNumber
instance DocType LineNumber where
    typeGetPrompt _ = "Line"
    
data RegexTag deriving Typeable
instance DocType RegexTag where
    typeGetPrompt _ = "Regex"
    
data FilePatternTag deriving Typeable
instance DocType FilePatternTag where
    typeGetPrompt _ = "FilePattern"

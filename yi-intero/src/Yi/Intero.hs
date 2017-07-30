{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Yi.Intero
-- Description : Intero support for Yi.
-- Copyright   : 2017 Jaro Reinders
-- License     : GPL-2
-- Maintainer  : yi-devel@googlegroups.com
--
-- This module implements actual Yi Actions using the hidden InteroAPI module.

module Yi.Intero (
    Intero
  , interoStart
  , interoLocAt, interoTypeAt, interoEval, interoUses, interoJump
  , interoModule
  ) where

import qualified Yi.Rope as R
import Control.Concurrent.MVar
import Data.Text (Text, unpack, pack)
import Data.Binary
import Yi.Types
import Yi.Region
import Data.Default (Default, def)
import Yi.Editor
import Control.Monad
import Yi.Buffer.Basic (BufferRef)
import Yi.Buffer.Misc
import Control.Monad.Base
import Yi.Buffer.Region
import Yi.Buffer.Normal
import Yi.Core
import Yi.Monad
import Data.Semigroup ((<>))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Data.Text (pack)
import Control.Exception (IOException, try)
import System.Directory (canonicalizePath)
import System.FilePath (isRelative, equalFilePath)
import Data.Char (isDigit, isSpace)
import Yi.File (openNewFile)
import Lens.Micro.Platform ((.~),(%~))
import Control.Category ((>>>))
import Yi.Keymap.Keys ((?>>!), spec, choice, important)
import Yi.Keymap (topKeymapA)
import Yi.Event (Key (KEsc,KEnter))

import qualified InteroAPI as Intero
import InteroAPI (Request)

-- | The main type that facilitates the communication with the background intero process.
-- Should be Nothing when intero is not yet started and Just an MVar that you can use
-- to make requests if intero is started.
--
-- This type is a YiVariable so we can use putEditorDyn when we start intero
-- and getEditorDyn whenever we need it.
newtype Intero = Intero { unIntero :: (Maybe (MVar Request)) }

instance Show Intero where
  show _ = "Intero"
instance Binary Intero where
  put _ = return ()
  get = return def
instance Default Intero where
  def = Intero Nothing
instance YiVariable Intero

-- | Start the Intero background process.
interoStart :: Action
interoStart = YiA $ do
  Intero intero <- getEditorDyn
  case intero of
    -- FIXME: change "." into the correct directory
    Nothing -> liftBase (try (Intero.start ".") :: IO (Either IOException (MVar Request)))
           >>= either (errorEditor . pack . show) (putEditorDyn . Intero . Just)
    Just _ -> errorEditor "Intero is already running"

-- | Pass a raw string to intero. Intero is like ghci in that you can
-- evaluate any haskell expression, but you can also use commands like
-- ":show modules" or ":show imports" like you would use them in ghci.
-- On top of all that Intero has implemented ide-like commands, like
-- ":type-at" and ":uses". Intero's github page has a small list of
-- features:
-- https://github.com/commercialhaskell/intero/blob/28271d50ca65c460cd0983cea13a2c4509b95583/TOOLING.md
interoEval :: String -> Action
interoEval command = YiA $
  either errorEditor return =<< runExceptT (do
    intero <- ExceptT $ maybe (Left "Intero not running") Right . unIntero <$> getEditorDyn
    res    <- liftBase $ Intero.eval intero command
    -- FIXME: close previous *intero* buffers before opening a new one.
    lift $ inNewBuffer "*intero*" (R.fromString res))

-- TODO: add possibility of using the current selection as input string to the intero command

-- | Intero's 'loc-at', 'type-at' and 'uses' commands all use the same file-range-name format
-- so I wrote a function to reduce code duplication.
interoFileRangeNameAction
  :: (MVar Request -> String -> (Int,Int,Int,Int) -> String -> IO String)
  -> Action
interoFileRangeNameAction f = YiA $
  either errorEditor return =<< runExceptT (do
    file   <- ExceptT $ maybe (Left "Not in file") Right <$> withCurrentBuffer (gets file)
    intero <- ExceptT $ maybe (Left "Intero not running") Right . unIntero <$> getEditorDyn
    region <- lift $ withCurrentBuffer (regionOfB unitWord)
    range  <- lift $ regionToRange region
    name   <- lift $ withCurrentBuffer (readRegionB region)
    res    <- liftBase (f intero file range (R.toString name))
    lift $ inNewBuffer "*intero*" (R.fromString res))

-- | Finds the location of the definition of the current word under the cursor.
-- This will open the result (just a string containing the module name) in a split window.
interoLocAt  :: Action
interoLocAt  = interoFileRangeNameAction Intero.locAt

-- | Finds the places where the current word under the cursor is used if it is a definition
-- or the place where it is defined if it isn't. This opens a new buffer in which the list
-- of found uses are displayed. Press enter to select one of them or escape to return to
-- the original buffer.
interoUses   :: Action
interoUses   = YiA $
  either errorEditor return =<< runExceptT (do
    file   <- ExceptT $ maybe (Left "Not in file") Right <$> withCurrentBuffer (gets file)
    intero <- ExceptT $ maybe (Left "Intero not running") Right . unIntero <$> getEditorDyn
    region <- lift $ withCurrentBuffer (regionOfB unitWord)
    range  <- lift $ regionToRange region
    name   <- lift $ withCurrentBuffer (readRegionB region)
    res    <- liftBase (Intero.uses intero file range (R.toString name))
    -- Parsing the result
    -- Intero returns "Couldn't resolve to any module." if the input was not found
    -- anywhere, so we just check if the output contains a colon.
    when (':' `notElem` res) $ throwE "Couldn't resolve to any modules."
    -- We can detect results of the form <packagename>-<packageversion>-<hash>:<module>
    -- by checking if the path is not absolute, because intero will (AFAIK) only return
    -- absolute paths if it returns a path.
    let (path,loc) = span (/= ':') res
    when (isRelative res) $ throwE "Definition is outside the current project."
    lift $ withEditor $ do
      b <- gets currentBuffer
      newBufferE (MemBuffer "*intero*") $ R.fromString res
      withCurrentBuffer $ modifyMode $ modeKeymapA .~ topKeymapA %~ important (choice [spec KEnter ?>>! jumpToUse b file, spec KEsc ?>>! EditorA (switchToBufferE b)])
               >>> modeNameA   .~ "intero-uses"
    )
  where
    jumpToUse :: BufferRef -> FilePath -> Action
    jumpToUse original file = YiA $ either errorEditor return =<< runExceptT (do
      res <- lift $ withCurrentBuffer $ R.toString <$> readUnitB Line
      let (path,loc) = span (/= ':') res
          (line,loc') = (\(a,b) -> (read a,b)) $ span isDigit $ dropWhile (not . isDigit) loc
          col = (flip (-) 1) $ read $ takeWhile isDigit $ dropWhile (not . isDigit) loc'
      path' <- liftBase $ canonicalizePath path
      file' <- liftBase $ canonicalizePath file
      if (path' `equalFilePath` file')
        then lift $ withEditor $ switchToBufferE original
        else lift $ openNewFile $ path'
      point <- lift $ withCurrentBuffer $ pointOfLineColB line col
      lift $ void $ withCurrentBuffer $ moveTo point
      )

-- | Find the type of the current word under the cursor.
interoTypeAt :: Action
interoTypeAt = interoFileRangeNameAction Intero.typeAt

-- | Convert a Yi Region to Intero's range format.
regionToRange :: Region -> YiM (Int,Int,Int,Int)
regionToRange region = withCurrentBuffer $ do
  line  <- lineOf $ regionStart region
  col   <- colOf  $ regionStart region
  line' <- lineOf $ regionEnd   region
  col'  <- colOf  $ regionEnd   region
  return (line,col,line',col')

-- | Show a YiString in a split window.
inNewBuffer :: Text -> R.YiString -> YiM ()
inNewBuffer bufName content = withEditor $ withOtherWindow $ do
  void $ newBufferE (MemBuffer bufName) $ content

-- | Jump to the definition of the word currently under the cursor. This fails if the
-- current word is not a function or if it is outside the current stack project.
-- If the function is outside the current file then it is opened in the current window.
interoJump :: Action
interoJump = YiA $
  either errorEditor return =<< runExceptT (do
    file   <- ExceptT $ maybe (Left "Not in file") Right <$> withCurrentBuffer (gets file)
    intero <- ExceptT $ maybe (Left "Intero not running") Right . unIntero <$> getEditorDyn
    region <- lift $ withCurrentBuffer (regionOfB unitWord)
    range  <- lift $ regionToRange region
    name   <- lift $ withCurrentBuffer (readRegionB region)
    res    <- liftBase (Intero.locAt intero file range (R.toString name))
    -- Parsing the result
    -- Intero returns "Couldn't resolve to any module." if the input was not found
    -- anywhere, so we just check if the output contains a colon.
    when (':' `notElem` res) $ throwE "Couldn't resolve to any modules."
    -- We can detect results of the form <packagename>-<packageversion>-<hash>:<module>
    -- by checking if the path is not absolute, because intero will (AFAIK) only return
    -- absolute paths if it returns a path.
    let (path,loc) = span (/= ':') res
    when (isRelative res) $ throwE "Definition is outside the current project."
    let (line,loc') = (\(a,b) -> (read a,b)) $ span isDigit $ dropWhile (not . isDigit) loc
        col = (flip (-) 1) $ read $ takeWhile isDigit $ dropWhile (not . isDigit) loc'
    path' <- liftBase $ canonicalizePath path
    file' <- liftBase $ canonicalizePath file
    unless (path' `equalFilePath` file') $ lift $ openNewFile $ path'
    point <- lift $ withCurrentBuffer $ pointOfLineColB line col
    lift $ void $ withCurrentBuffer $ moveTo point
    )

-- | Jump to a module inside the current stack package. This opens the module (if it exists)
-- in the current window.
interoModule :: String -> Action
interoModule moduleName = YiA $
  either errorEditor return =<< runExceptT (do
    intero <- ExceptT $ maybe (Left "Intero not running") Right . unIntero <$> getEditorDyn
    res    <- liftBase $ Intero.eval intero ":show modules"
    let table = map ((\(a,b) -> (a, takeWhile (/= ',') $ dropWhile isSpace $ tail $ dropWhile isSpace b)) . span (/= ' ')) $ lines res
    case lookup moduleName table of
      Nothing -> throwE "Module not in project."
      Just path -> lift $ openNewFile $ path
    )
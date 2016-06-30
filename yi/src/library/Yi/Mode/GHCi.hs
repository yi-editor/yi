{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Mode.GHCi
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A mode for GHCi, implemented as tweaks on Interaction mode

module Yi.Mode.GHCi where

import           GHC.Generics (Generic)

import           Control.Lens         (makeLenses, (%~), (&), (.~))
import           Data.Binary          (Binary (..))
import           Data.Default         (Default (..))
import           Data.Text            ()
import qualified Data.Text            as T (findIndex)
import           Data.Typeable        (Typeable)
import           Yi.Buffer
import           Yi.Keymap            (YiM, topKeymapA)
import           Yi.Keymap.Keys       (Key (KHome), important, spec, (?>>!))
import           Yi.Lexer.Alex        (Tok)
import           Yi.Lexer.Compilation (Token ())
import qualified Yi.Mode.Interactive  as I (mode, spawnProcessMode)
import qualified Yi.Rope              as R (toText)
import           Yi.Syntax.OnlineTree (Tree)
import           Yi.Types             (YiVariable)

-- | The process name to use to spawn GHCi.
data GhciProcessName = GhciProcessName
  { _ghciProcessName :: FilePath
    -- ^ Command to run when spawning GHCi.
  , _ghciProcessArgs :: [String]
    -- ^ Args to pass to the process.
  } deriving (Typeable, Show, Generic)

-- | The process name defaults to @ghci@.
instance Default GhciProcessName where
  def = GhciProcessName { _ghciProcessName = "ghci"
                        , _ghciProcessArgs = []
                        }

instance Binary GhciProcessName

makeLenses ''GhciProcessName

-- | Setting this is a bit like '(setq haskell-program-name foo)' in
-- emacs' @haskell-mode@.
instance YiVariable GhciProcessName

-- | Mode used for GHCi. Currently it just overrides 'KHome' key to go
-- just before the prompt through the use of 'homeKey'.
mode :: Mode (Tree (Tok Token))
mode = I.mode
  & modeNameA .~ "ghci"
  & modeKeymapA .~ topKeymapA %~ important (spec KHome ?>>! homeKey)

-- | The GHCi prompt always begins with ">"; this goes to just before
-- it, or if one is already at the start of the prompt, goes to the
-- beginning of the line. (If at the beginning of the line, this
-- pushes you forward to it.)
homeKey :: BufferM ()
homeKey = readLnB >>= \l -> case T.findIndex ('>' ==) (R.toText l) of
  Nothing -> moveToSol
  Just pos -> do
    (_,mypos) <- getLineAndCol
    moveToSol >> if mypos == (pos + 2)
                 then return ()
                 else moveXorEol (pos + 2)

-- | Spawns an interactive process ("Yi.Mode.Interactive") with GHCi
-- 'mode' over it.
spawnProcess :: FilePath -- ^ Command to use.
             -> [String] -- ^ Process args.
             -> YiM BufferRef -- ^ Reference to the spawned buffer.
spawnProcess = I.spawnProcessMode mode

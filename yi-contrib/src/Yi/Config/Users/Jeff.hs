{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
     FunctionalDependencies, GeneralizedNewtypeDeriving,
     MultiParamTypeClasses, TypeSynonymInstances #-}
module Yi.Config.Users.Jeff (myConfig) where

import Yi.Prelude
import Prelude ()

import Yi

import Yi.Keymap.Vim
import Yi.Snippets
import Yi.Snippets.Haskell
-- import Yi.UI.Pango as Pango
-- import Yi.UI.Vty as Vty

myConfig :: Config
myConfig = defaultVimConfig
  { defaultKm = myVimKeymap
  , configUI = (configUI defaultVimConfig)
    { configTheme = defaultTheme
    , configWindowFill = '~'
    }
  , startActions = [makeAction (maxStatusHeightA %= 20 :: EditorM ())]
  -- , startFrontEnd = Pango.start
  }

myVimKeymap = mkKeymap $ defKeymap `override` \super self -> super
  { v_top_level = v_top_level super ||>
      (char ';' ?>>! resetRegexE)

  , v_ins_char  = (v_ins_char super ||> tabKeymap) <|>
      choice [ ctrlCh 's' ?>>! moveToNextBufferMark deleteSnippets
             , meta (spec KLeft)  ?>>! prevWordB
             , meta (spec KRight) ?>>! nextWordB
             ]
  }

deleteSnippets = True

tabKeymap = superTab True $ fromSnippets deleteSnippets $
  [ ("f", hsFunction)
  , ("c", hsClass)
  ]

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Keymap.Cua.Utils
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is aimed at being a helper for the Cua keybindings.
-- In particular this should be useful for anyone that has a custom
-- keymap derived from or based on the Emacs one.

module Yi.Keymap.Cua.Utils
  ( askQuitEditor
  , isearchKeymap
  , findFile
  )
where

import           Control.Applicative (Alternative ((<|>), many))
import           Control.Monad       (filterM, void)
import           Control.Monad.Base  ()
import           Data.Monoid         ((<>))
import qualified Data.Text           as T (Text, concat, singleton, unpack)
import           System.FriendlyPath ()
import           Yi.Buffer
import           Yi.Core             (quitEditor)
import           Yi.Editor
import           Yi.File             (deservesSave, fwriteBufferE, openingNewFile)
import           Yi.Keymap           (Keymap, YiM, write)
import           Yi.Keymap.Keys
import           Yi.MiniBuffer
import           Yi.Misc             (promptFile)
import           Yi.Monad            (gets)
import           Yi.Search


----------------------------
-- | Quits the editor if there are no unmodified buffers
-- if there are unmodified buffers then we ask individually for
-- each modified buffer whether or not the user wishes to save
-- it or not. If we get to the end of this list and there are still
-- some modified buffers then we ask again if the user wishes to
-- quit, but this is then a simple yes or no.
askQuitEditor :: YiM ()
askQuitEditor = askIndividualSave True =<< getModifiedBuffers

getModifiedBuffers :: YiM [FBuffer]
getModifiedBuffers = filterM deservesSave =<< gets bufferSet

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
  yesAction = do void $ fwriteBufferE (bkey firstBuffer)
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


-- | Finds file and runs specified action on the resulting buffer
findFileAndDo :: T.Text -- ^ Prompt
              -> BufferM a -- ^ Action to run on the resulting buffer
              -> YiM ()
findFileAndDo prompt act = promptFile prompt $ \filename -> do
  printMsg $ "loading " <> filename
  openingNewFile (T.unpack filename) act

-- | Open a file using the minibuffer. We have to set up some stuff to
-- allow hints and auto-completion.
findFile :: YiM ()
findFile = findFileAndDo "find file:" $ return ()


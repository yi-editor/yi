module YiConfig (yiMain) where

import Yi.Yi
import Yi.Editor
import Yi.Keymap.Emacs
import Control.Monad.Trans
import Yi.Buffer

yiMain :: YiM ()
yiMain = do
-- Create an entire new keymap; the only thing it does is on 'h', insert the
-- string "Hello" into the buffer - and nothing else.
  changeKeymapE $ event (Event (KASCII 'h') []) >> write (insertN "hello")
                    <|| keymap
  msgE "User configuration successful."


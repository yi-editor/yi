module Main where

import Yi
import Yi.Keymap
import Yi.Keymap.Vim 
import Yi.Snippets

main :: IO ()
main = yi $ defaultVimConfig {
    defaultKm = myVimKeymap

    -- install update handler for dependent snippet regions
    bufferUpdateHandler = [updateUpdatedMarks] 
}

deleteSnippets = False

myVimKeymap = mkKeymap $ defKeymap `override` \super self -> super {
          v_ins_char  = (v_ins_char super ||> tabKeymap)
                       <|> (ctrlCh 's' ?>>! moveToNextBufferMark deleteSnippets)
        , v_ex_cmds = myExCmds
        }
        
myExCmds = exCmds [ ("sd", const $ withEditor showDepMarks, Nothing) ]
                  
showDepMarks = withBuffer0 (getA bufferDynamicValueA >>= 
                            mapM markRegion . concat . marks) >>=
               printMsg . show

tabKeymap = superTab True $ fromSnippets deleteSnippets $
    [ ("test", testSnippet)
    , ("hc", haskellComment)
    , ("ts2", testSnippet2)
    , ("ts3", testSnippet3)
    , ("ts4", testSnippet4)
    ]

haskellComment :: SnippetCmd ()
haskellComment = snippet $
    "{-" & (cursor 1) & "\n-}"

testSnippet :: SnippetCmd ()
testSnippet = snippet $
    "if ( " & (cursorWith 1 "...") & " ) {\n" &
    "\t" & (cursorWith 2 "/* code */") &
    "\n}\n" & (cursor 3)
    
testSnippet2 :: SnippetCmd ()
testSnippet2 = snippet $
  (cursorWith 2 "abcdef") & "\n" &
  (dep 2) & "\ndef" &
  (cursor 1)

testSnippet3 :: SnippetCmd ()
testSnippet3 = snippet $ 
    "for(int " & (cursorWith 2 "ab") & " = 0; " &
         (dep 2) & " < " & (cursorWith 1 "arr") & ".length;" &
         (dep 2) & "++) {\n" &
    "\t" & (cursorWith 3 "/* code */") &
    "\n}\n" & (cursor 4)

testSnippet4 :: SnippetCmd ()
testSnippet4 = snippet $ 
    "for(int " & (cursorWith 1 "ab") & " = 0; " &
         (dep 1) & " < " & (cursorWith 2 "arr") & ".length;" &
         (dep 1) & "++) {\n" &
    "\t" & (cursorWith 3 "/* code */") &
    "\n}\n" & (cursor 4)
  

module Yi.Config.Default.MiscModes (configureMiscModes) where

import Yi.Config.Simple    (ConfigM, addMode)
import Yi.Modes

configureMiscModes :: ConfigM ()
configureMiscModes = do
  addMode cMode
  addMode objectiveCMode
  addMode cppMode
  addMode cabalMode
  addMode clojureMode
  addMode srmcMode
  addMode gitCommitMode
  addMode svnCommitMode
  addMode ocamlMode
  addMode perlMode
  addMode rubyMode
  addMode pythonMode
  addMode jsonMode
  addMode gnuMakeMode
  addMode ottMode
  addMode whitespaceMode
  addMode markdownMode

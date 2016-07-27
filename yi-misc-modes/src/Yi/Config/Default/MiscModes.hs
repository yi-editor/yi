module Yi.Config.Default.MiscModes (configureMiscModes) where

import Lens.Micro.Platform ((%=))
import Yi.Config.Simple    (ConfigM, addMode)
import Yi.Config.Lens      (modeTableA)
import Yi.Modes
import Yi.Types            (AnyMode (..), Mode)

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

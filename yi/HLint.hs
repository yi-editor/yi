import "hint" HLint.Default
import "hint" HLint.Builtin.All

ignore "Eta reduce"

error = x `mappend` y `mappend` z  ==> mconcat [x, y, z]

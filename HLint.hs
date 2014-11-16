import "hint" HLint.Default
import "hint" HLint.Builtin.All

ignore "Eta reduce"

error   "use mconcat"                            = x `mappend` y `mappend` z   ==> mconcat [x, y, z]
warning "use concat"                             = x ++ y ++ z   ==> concat [x, y, z]
warning "use fromMaybe"                          = \case {Nothing -> a; Just b -> b} ==> fromMaybe a
error   "putEditorDynA works on any MonadEditor" = withEditor $ putEditorDynA a ==> putEditorDynA a
error   "getEditorDynA works on any MonadEditor" = withEditor $ getEditorDynA a ==> getEditorDynA a
error   "Why not use <$>?"                       = a          >>= return . b             ==> b <$> a
error   "Why not use <$>?"                       = return . b =<< a                      ==> b <$> a


import Data.Maybe
import Yi.Keymap.Vim2.Ex

commands :: [(String, ExCommand)]
commands =
    [ ("d", ExDelete)
    , ("delete", ExDelete)
    , ("g/foo/d", ExGlobal "foo" False ExDelete)
    , ("g/foo/s/123/234/", ExGlobal "foo" False (ExReplace "123" "234" []))
    , ("global/quux/d", ExGlobal "quux" False ExDelete)
    , ("g!/foo/d", ExGlobal "foo" True ExDelete)
    , ("vg/foo/d", ExGlobal "foo" True ExDelete)
    , ("global!/foo/d", ExGlobal "foo" True ExDelete)
    , ("vglobal/foo/delete", ExGlobal "foo" True ExDelete)
    , ("s/foo/bar/g", ExReplace "foo" "bar" [Global])
    ]

main :: IO ()
main = mapM_ putStrLn . mapMaybe checkCommand $ commands

checkCommand :: (String, ExCommand) -> Maybe String
checkCommand (s, cmd) =
    case stringToExCommand s of
        Left e -> Just $ "Error while parsing \"" ++ s ++ "\": \n" ++ show e
        Right cmd' -> if cmd' == cmd
                      then Nothing
                      else Just $ s ++ " parsed to " ++ show cmd' ++
                                  ", not " ++ show cmd


import Data.Maybe
import Yi.Keymap.Vim2.Ex

fullCommands :: [String]
fullCommands =
    [ "delete"
    , "quit"
    , "quit!"
    , "quitall"
    , "quitall!"
    , "wquit"
    , "wquit!"
    , "wquitall!"
    , "edit foo"
    , "tabedit foo"
    , "write foo"
    , "write"
    , "writeall"
    ]

main :: IO ()
main = mapM_ putStrLn . mapMaybe checkCommand $ fullCommands

checkCommand :: String -> Maybe String
checkCommand s =
    case stringToExCommand allExCommands s of
        Nothing -> Just $ "Error while parsing \"" ++ s ++ "\""
        Just cmd -> if s == show cmd
                      then Nothing
                      else Just $ s ++ " parsed to " ++ show cmd

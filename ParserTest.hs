import Parser.Incremental
import System.Environment
import Yi.Syntax.Haskell
import Yi.Syntax
import Yi.Lexer.Haskell
import System.FilePath.Posix
import System.Directory
import Control.Monad
import qualified Yi.Lexer.Alex as Alex

haskellLexer = Alex.lexScanner alexScanToken initState 

main = do
    arg <- getArgs
    case arg of
        [] -> putStrLn "-d for all files in given directory, otherwise a list of files..."
        ("-d":d:xs) ->pFile =<< filterM doesFileExist . filter ((==) ".hs" . takeExtension) 
                             . map ((</>) d) =<< getDirectoryContents d
        _ -> pFile arg
  where pFile arg = do
            input <- mapM readFile arg
            mapM_ parse'' $ zip arg input

parse'' (fName, input) = do
    let getText = Scanner 0 id (error "getText: no character beyond eof")
                     (\idx -> zip [idx..] (drop (fromIntegral idx) input))
        scanner = (indentScanner . haskellLexer) getText        
    let allSyms = map snd $ scanRun scanner $ scanInit scanner
    let finalResult = evalL $ pushEof $ pushSyms allSyms $ mkProcess parse
    putStrLn $ fName ++ " # " ++ show finalResult

import Parser.Incremental
import System.Environment
import Yi.Syntax.Haskell
import Yi.Syntax
import Yi.Lexer.Haskell
import System.FilePath.Posix
import System.Directory
import Control.Monad
import Data.Foldable
import Data.List
import qualified Yi.Lexer.Alex as Alex

haskellLexer = Alex.lexScanner alexScanToken initState 

main = do
    arg <- getArgs
    let (flags,rest) = partition (\d -> ("-r" == d) ||("-d" == d)) arg 
    case arg of -- partition
        [] -> putStrLn "-d for debug mode, -r for recursive, otherwise a list of files/folders..."
        _ -> dir flags rest (pFile flags)
  where pFile flags arg' = do
            b <-foldlM (\a b -> liftM2 (==) (return a) (doesFileExist b)) True arg'
            case b of
                True -> do
                    input <- mapM readFile arg'
                    Control.Monad.mapM_ (parse'' flags) $ zip arg' input
                False -> putStrLn $ (++) "List contains non existing file" (show arg')

dir _ [] _ = return ()
dir flags (d:ds) fun = do
    b <- doesDirectoryExist d
    let b' = b &&(Data.List.all ((/=) '.') d)
    case b' of
        True -> do
            (fun =<< filterM doesFileExist . filter ((==) ".hs" . takeExtension)
                   . map ((</>) d) =<< getDirectoryContents d)
            recurse
            dir flags ds fun
        False -> if(b /= b') then dir flags ds fun else (fun [d]) >> dir flags ds fun
  where recurse = if (Data.List.elem "-r" flags) then ((\x -> dir flags x (fun)) =<< filterM (doesDirectoryExist) 
                      =<< liftM2 map (return $ (</>) d) (getDirectoryContents d)) else return ()

parse'' flags (fName, input) = do
    let getText = Scanner 0 id (error "getText: no character beyond eof")
                     (\idx -> zip [idx..] (drop (fromIntegral idx) input))
        scanner = (indentScanner . haskellLexer) getText        
    let allSyms = map snd $ scanRun scanner $ scanInit scanner
    let finalResult = evalL $ pushEof $ pushSyms allSyms $ mkProcess parse
    putStrLn $ fName ++ " # " ++ show finalResult
    if (Data.List.elem "-d" flags) then print allSyms else return ()

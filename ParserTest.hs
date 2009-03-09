import Parser.Incremental
import System.Environment
import qualified Yi.Syntax.Haskell as Haskell
import qualified Yi.Syntax.Paren as Paren
import qualified Yi.Syntax.JavaScript as JS
import Yi.Syntax
import Yi.Lexer.Haskell
import qualified Yi.Lexer.JavaScript as JLex
import System.FilePath.Posix
import System.Directory
import Control.Monad
import Data.Foldable
import Data.List
import qualified Yi.Lexer.Alex as Alex
import Data.Time

haskellLexer = Alex.lexScanner alexScanToken initState 

main = do
    arg <- getArgs
    let (flags,rest) = partition (isPrefixOf "-") arg  -- flag begins with -
    case arg of
        [] -> putStrLn $ "-js for javascript\n-hs for haskell\n-d for debug mode\n" ++ 
                         "-r for recursive\notherwise a list of files/folders...\n" ++
                         "-cmp to compare with Paren.hs parse\n-oneBy to parse one token at a time"
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
            (fun =<< filterM doesFileExist . filter ((==) getExt . takeExtension)
                   . map ((</>) d) =<< getDirectoryContents d)
            recurse
            dir flags ds fun
        False -> if(b /= b') then dir flags ds fun else (fun [d]) >> dir flags ds fun
  where recurse = if (Data.List.elem "-r" flags) then ((\x -> dir flags x (fun)) =<< filterM (doesDirectoryExist) 
                      =<< liftM2 map (return $ (</>) d) (getDirectoryContents d)) else return ()
        getExt = if (Data.List.elem "-hs" flags) then ".hs" else ".js"

parse'' flags (fName, input) = do
    if (Data.List.elem "-cmp" flags) then
       print =<< (liftM2 (-) (pp (mkProcess Haskell.parse) Haskell.indentScanner) (pp (mkProcess Paren.parse) Paren.indentScanner))
       else pp (mkProcess Haskell.parse) Haskell.indentScanner >> return ()
  where 
      pp proc scan = do
          let allSyms = getSyms scan
          t <- getCurrentTime
          let finalResult = option proc allSyms
          -- let finalResult = evalL $ pushEof $ pushSyms allSyms $ mkProcess parse -- old stuff
          putStrLn $ fName ++ " # " ++ show finalResult
          t' <- getCurrentTime
          if (Data.List.elem "-d" flags) then do
              print allSyms
              return $ diffUTCTime t' t
              else return $ diffUTCTime t' t
      getSyms p = 
          let getText = Scanner 0 id (error "getText: no character beyond eof")
                         (\idx -> zip [idx..] (drop (fromIntegral idx) input))
              scan = (p . haskellLexer) getText        
          in map snd $ scanRun scan $ scanInit scan
      option thisFar xs = if (Data.List.elem "-oneBy" flags) then
                              oneByOne thisFar xs
                              else evalL $ pushEof $ pushSyms xs $ thisFar

oneByOne thisFar [] = evalL $ pushEof thisFar
oneByOne thisFar (h:rest) = do
    let res = pushSyms [h] thisFar
    let eRes = oneByOne res []
    case (isPrefixOf "!^<>1, " (show eRes)) of
        True -> eRes
        False -> oneByOne res rest -- if no errors then keep parsing




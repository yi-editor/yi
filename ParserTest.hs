import Parser.Incremental
import System.Environment
import qualified Yi.Syntax.Haskell as Haskell
import qualified Yi.Syntax.Paren as Paren
import qualified Yi.Syntax.JavaScript as JS
import Yi.Syntax
import Yi.Lexer.Haskell
import qualified Yi.Lexer.JavaScript as JSLex
import qualified Yi.IncrementalParse as IncrParser
import System.FilePath.Posix
import System.Directory
import Control.Monad
import qualified Data.Foldable as P
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
        _ -> mapM_ (dirs flags) rest

dirs flags dir = do
    b <- doesDirectoryExist dir
    let b' = b &&(Data.List.all ((/=) '.') dir)
    case b' of
        True -> (pFiles =<< getFiles) >> recurse
        False -> pFiles [dir]
  where recurse = if (Data.List.elem "-r" flags) then ((\x -> mapM_ (dirs flags) x) =<< filterM (doesDirectoryExist) 
                      =<< liftM2 map (return $ (</>) dir) (getDirectoryContents dir)) else return ()
        getExt = if (Data.List.elem "-hs" flags) then ".hs" else ".js"
        getFiles = (filterM doesFileExist . filter ((==) getExt . takeExtension)
                   . map ((</>) dir) =<< getDirectoryContents dir)
        pFiles arg' = do
            b <-P.foldlM (\a b -> liftM2 (==) (return a) (doesFileExist b)) True arg'
            case b of
                True -> do
                    input <- mapM readFile arg'
                    Control.Monad.mapM_ (fParse flags) $ zip arg' input
                False -> putStrLn $ (++) "List contains non existing file" (show arg')

fParse flags (fName, input) = do
    if (Data.List.elem "-cmp" flags) then -- compare with Paren.hs
       print =<< (liftM2 (-) (mParse (mkProcess Haskell.parse) Haskell.indentScanner) (mParse (mkProcess Paren.parse) Paren.indentScanner))
       else mParse (mkProcess Haskell.parse) Haskell.indentScanner >> return () -- otherwise just run the parser
  where 
      mParse proc scan = do
           let allSyms = getSyms scan
           time <- getCurrentTime
           let finalResult = option proc allSyms
           putStrLn $ fName ++ " # " ++ (show $ evalL finalResult)
           time' <- getCurrentTime
           if (Data.List.elem "-d" flags) then do
               putStrLn $ "Shifts: " ++ (show $ shifts finalResult)
               putStrLn $ "Disjunctions: " ++ (show $ best finalResult)
               print allSyms
               return $ diffUTCTime time' time
               else return $ diffUTCTime time' time
      getSyms p = 
          let getText = Scanner 0 id (error "getText: no character beyond eof")
                         (\idx -> zip [idx..] (drop (fromIntegral idx) input))
              scan = (p . haskellLexer) getText        
          in map snd $ scanRun scan $ scanInit scan
      option thisFar xs = if (Data.List.elem "-oneBy" flags) then -- Choose to go stepwise or all in
                              oneByOne thisFar xs
                              else pushEof $ pushSyms xs $ thisFar

oneByOne thisFar [] = pushEof thisFar
oneByOne thisFar (h:rest) = do
    let res = pushSyms [h] thisFar
    let eRes = oneByOne res []
    oneByOne res rest -- TODO: add check for errors and stop then?

shifts s = length $ filter ('>' ==) (show s)

best s = length $ filter ('(' ==) (show s)
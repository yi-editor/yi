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
import Data.List as List
import qualified Yi.Lexer.Alex as Alex
import Data.Time

haskellLexer = Alex.lexScanner alexScanToken initState
jsLexer = Alex.lexScanner JSLex.alexScanToken JSLex.initState

main = do
    arg <- getArgs
    let (flags,rest) = partition (isPrefixOf "-") arg  -- flag begins with -
    case arg of
        [] -> putStrLn $ "-js for javascript\n-hs for haskell\n-d for debug mode\n" ++
                         "-r for recursive\notherwise a list of files/folders...\n" ++
                         "-cmp to compare with Paren.hs parse\n-oneBy to parse one token at a time"
        _ -> mapM_ (dirs flags) rest

-- | Find files to parse, recursively if flagged -r
dirs flags dir = do
    b <- doesDirectoryExist dir
    let b' = b &&(List.all ((/=) '.') dir) -- dont look in "." or ".."
    case b' of
        True -> (pFiles =<< getFiles) >> recurse
        False -> pFiles [dir] -- check so that file has correct extension
  where recurse = if (List.elem "-r" flags) then ((\x -> mapM_ (dirs flags) x) =<< filterM (doesDirectoryExist) 
                      =<< liftM2 map (return $ (</>) dir) (getDirectoryContents dir)) else return ()
        getExt = if (List.elem "-hs" flags) then [".hs"] else [".js",".json"] -- Should take more js files
        getFiles = (filterM doesFileExist . filter (\x -> elem (takeExtension x) getExt )
                   . map ((</>) dir) =<< getDirectoryContents dir)
        pFiles arg' = do
            b <-P.foldlM (\a b -> liftM2 (==) (return a) (doesFileExist b)) True arg'
            case b of
                True -> do
                    input <- mapM readFile arg'
                    Control.Monad.mapM_ (parse flags) $ zip arg' input
                False -> return () -- not valid file/folder

-- | Parse given flags, file name and content
parse flags (fName, input) = do
    if (List.elem "-cmp" flags) then do -- compare with Paren.hs
--        t <-getCurrentTime
       let tokList = getSyms (Haskell.indentScanner . haskellLexer)
           (paths, finRes) = option (mkProcess Haskell.parse) tokList
--        putStrLn $ "Haskell: " ++ fName ++ " # " ++ (show $ evalL finRes)
--        t' <- getCurrentTime
       let tokList' = getSyms (Paren.indentScanner . haskellLexer)
           (paths', finRes') = option (mkProcess Paren.parse) tokList'
--        putStrLn $ "Paren:   " ++ fName ++ " # " ++ (show $ evalL finRes')
--        tt' <- getCurrentTime
--        print $ (-) (diffUTCTime t' t) (diffUTCTime tt' t')
       debug $ getSyms (Haskell.indentScanner . haskellLexer)
       mapM_ print (zip3 paths paths' tokList)
       putStrLn $ "Different paths (Haskell,Paren): " ++ (show $ zip paths paths')
       putStrLn $ "Haskell: " ++ fName ++ " # " ++ (show $ evalL finRes)
       putStrLn $ "Paren:   " ++ fName ++ " # " ++ (show $ evalL finRes')
       else if (List.elem "-hs" flags) then do
                 let tokList = getSyms (Haskell.indentScanner . haskellLexer)
                     (r,info) = option (mkProcess Haskell.parse) tokList -- otherwise just run the parser
                 mapM_ print r
                 putStrLn $ show $ evalL info
                 else do
                     let tokList = getSyms jsLexer
                         (r,info) = option (mkProcess JS.parse) tokList
                     mapM_ print (zip r tokList) -- will print width followed by token added
                     putStrLn $ show $ evalL info
  where
      debug allSyms = if (List.elem "-d" flags) then
                           mapM_ print allSyms
                           else return ()
      getSyms scan =
          let getText = Scanner 0 id (error "getText: no character beyond eof")
                         (\idx -> zip [idx..] (drop (fromIntegral idx) input))
              scan' = scan getText
          in map snd $ scanRun scan' $ scanInit scan'
      option thisFar xs = if (List.elem "-oneBy" flags) then -- Choose to go stepwise or all in
                              oneByOne thisFar xs
                              else ([],pushEof $ pushSyms xs $ thisFar)

-- | Push the symbols into the parser one by one
-- Send around the different number of (Best)
oneByOne thisFar [] = let p = pushEof thisFar in ([], p)
oneByOne thisFar (h:rest) = 
    let res = pushSyms [h] thisFar
        eRes = evalL res
        (i, p') = oneByOne eRes rest
    in  ((countWidth res):i,p')

-- best s = length $ filter ('^' ==) (show s)

best s = List.foldl (\p q -> p + value q) 0 (show s)

value '^' = 1
value _ = 0
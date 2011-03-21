import Parser.Incremental
import System.Environment
import qualified Yi.Syntax.Haskell as Haskell
import qualified Yi.Syntax.Paren as Paren
import qualified Yi.Syntax.JavaScript as JS
import Yi.Syntax
import Yi.Lexer.Haskell
import Yi.Lexer.Alex
import qualified Yi.Lexer.JavaScript as JSLex
import qualified Yi.IncrementalParse as IncrParser
import System.FilePath.Posix
import System.Directory
import Control.Monad
import qualified Data.Foldable as P
import Data.List as List
import qualified Yi.Lexer.Alex as Alex
import Data.Tree

haskellLexer = Alex.lexScanner alexScanToken initState
jsLexer = Alex.lexScanner JSLex.alexScanToken JSLex.initState

main :: IO ()
main = do
    arg <- getArgs
    let (flags,rest) = partition (isPrefixOf "-") arg  -- flag begins with -
    case arg of
        [] -> putStrLn . unlines $ [ "-js for javascript\n-hs for haskell"
                                   , "-d for debug mode"
                                   , "-r for recursive"
                                   , "otherwise a list of files/folders..."
                                   , "-cmp to compare with Paren.hs parse"
                                   , "-oneBy to parse one token at a time"
                                   , "-Tree=<file> to output the tree in \"dot\" format to <file> (possibly truncated)"
                                   , "-Toks=<num> to parse `num' tokens of the input."
                                   ]
        _ -> mapM_ (dirs flags) rest

-- | Find files to parse, recursively if flagged -r
dirs :: [String] -> FilePath -> IO ()
dirs flags dir = do
    dirExist <- doesDirectoryExist dir
    if dirExist &&(List.all ((/=) '.') dir) then
       (pFiles =<< getFiles) >> recurse
       else pFiles [dir]
  where recurse :: IO ()
        recurse = when (List.elem "-r" flags) $
                     (mapM_ (dirs flags) =<<
                      (filterM doesDirectoryExist) . map ((</>) dir)
                      =<< (getDirectoryContents dir))
        getExt :: [String]
        getExt = if (List.elem "-hs" flags) then [".hs"] else [".js",".json"]
        getFiles :: IO [FilePath]
        getFiles = (filterM doesFileExist . filter (flip elem getExt . takeExtension)
                   . map ((</>) dir) =<< getDirectoryContents dir)
        pFiles :: [FilePath] -> IO ()
        pFiles arg' = do
            fileExist <-P.foldlM (\a b -> liftM2 (==) (return a) (doesFileExist b)) True arg'
            when fileExist $ do
                        input <- mapM readFile arg'
                        Control.Monad.mapM_ (parse flags) $ zip arg' input

extract ::(Tok t) -> String -> String
extract t = take (fromIntegral $ tokLen t) . drop (fromIntegral $ posnOfs $ tokPosn t)

-- | Parse given flags, file name and content
parse :: [String] -> (String, String) -> IO ()
parse flags (fName, input) = do
    if (List.elem "-cmp" flags) then do -- compare with Paren.hs
       let tokList = getSyms (Haskell.indentScanner . haskellLexer)
           (paths, finRes) = option (mkProcess Haskell.parse) tokList
           tokList' = getSyms (Paren.indentScanner . haskellLexer)
           (paths', finRes') = option (mkProcess Paren.parse) tokList'
       debug $ getSyms (Haskell.indentScanner . haskellLexer)
       write tokList (fullLog finRes) -- write the dot file version of the tree if correct flag
       mapM_ print (zip3 paths paths' (zip tokList (map (\t -> extract t input) tokList)))
       putStrLn $ "Different paths (Haskell,Paren): " ++ (show $ zip paths paths')
       putStrLn $ "Haskell: " ++ fName ++ " # " ++ (show $ evalL $ pushEof finRes)
       putStrLn $ "Paren:   " ++ fName ++ " # " ++ (show $ evalL $ pushEof finRes')
       else if (List.elem "-hs" flags) then do
                 let tokList = getSyms (Haskell.indentScanner . haskellLexer)
                     (r,info) = option (mkProcess Haskell.parse) tokList -- otherwise just run the parser
                 write tokList (fullLog info) -- write the dot file version of the tree if correct flag
                 mapM_ print (zip3 r tokList (map (\t -> extract t input) tokList)) -- will print width followed by token added
                 putStrLn $ fName ++ " :" ++ (show $ evalL $ pushEof info)
                 else do
                     let tokList = getSyms jsLexer
                         (r,info) = option (mkProcess JS.parse) tokList
                     write tokList (fullLog info) -- write the dot file version of the tree if correct flag
                     mapM_ print (zip3 r tokList (map (\t -> extract t input) tokList)) -- will print width followed by token added
                     putStrLn $ show $ evalL $ pushEof info
  where
      debug allSyms = when (List.elem "-d" flags) $
                           mapM_ print allSyms
      getSyms scan =
          let getText = Scanner 0 id (error "getText: no character beyond eof")
                         (\idx -> zip [idx..] (drop (fromIntegral idx) input))
              scan' = scan getText
          in numToks $ map snd $ scanRun scan' $ scanInit scan'
      option thisFar xs = if (List.elem "-oneBy" flags) then -- Choose to go stepwise or all in
                              oneByOne thisFar xs
                              else ([], pushSyms xs $ thisFar)
      write toks ~(msgs,log) = case (find (isPrefixOf "-Tree=") flags) of
          Nothing -> return ()
          Just x ->do print msgs
                      writeTree toks (drop 6 x) log
      numToks toks = case (find (isPrefixOf "-Toks=") flags) of
          Nothing -> toks
          Just x -> take (read (drop 6 x) :: Int) toks

-- | Push the symbols into the parser one by one
-- Send around the different number of (Best)
oneByOne thisFar [] = let p = pushEof thisFar in ([], thisFar) -- should be p later
oneByOne thisFar (h:rest) =
    let res = pushSyms [h] thisFar
        eRes = evalL res
        (i, p') = oneByOne eRes rest
    in  ((countWidth res):i,p')

-- | Dot functions
fromTree :: Tree (LogEntry,Int) ->[String]
fromTree node
    = let r = map edge (subForest node)
          rest = (map fromTree (subForest node))
      in (name : r ++ (concat rest))
 where edge n = "        " ++ (show $ snd $ rootLabel node)
                 ++ " -> " ++ (show $ snd $ rootLabel n)
       name = (show $ snd $ rootLabel node) ++" [style=filled,color="++ toColor (fst $ rootLabel node) ++ " ,label=" ++showLog (fst $ rootLabel node) ++ " ]"

-- | Replace the Shifts with the token it represents
shift' :: Show a => [a] -> Tree (LogEntry,Int) -> Tree (LogEntry,Int)
shift' (x:toks) (Node (LShift,n) trees) = (Node ((LS $ show x),n) (map (shift' toks) trees))
shift' toks (Node r trees)          = (Node r (map (shift' toks) trees))

toColor :: LogEntry -> String
toColor s = case s of
    LEmpty -> "lightgray"
    LDislike -> "yellow"
    LShift -> "cyan"
    LDone -> "lightseagreen"
    LFail -> "red"
    LSusp -> "green"
    (LLog str) -> "orange1"
    (LS str) -> "cyan"

showLog :: LogEntry -> String
showLog p = case p of
    (LLog str) -> "\"" ++ str ++ "\""
    (LS str)   -> "\"" ++ helper str ++ "\""
    p          -> show p
  where helper p = drop 2 $ filter ((/=) '\"') $ dropWhile ((/=) ':') p

-- | Write the file
writeTree :: Show a => [a] -> String -> Tree LogEntry -> IO ()
writeTree toks fName r = writeFile fName addBegEnd
    where addBegEnd = unlines $ ["digraph G {"]
                         ++ take 3000 (fromTree $ (shift' toks) $ (\(x,y)-> x)(numTree (r,0))) -- dont create too big trees
                         ++ ["}"]

-- | Give unique number to each tree, also put the added token into the Tree
numTree ::((Tree LogEntry),Int) ->((Tree (LogEntry,Int)),Int)
numTree ((Node name []),n) = ((Node (name,n) []),n+1)
numTree ((Node name (x:[]),n)) = let (x',n') = numTree (x,n)
                                 in ((Node (name,n') [x']),n'+1)
numTree ((Node name (x:xx:[])),n) = let (x',n') = numTree (x,n)
                                        (x'',n'') = numTree (xx,n')
                                     in ((Node (name, n'') (x':[x''])),n''+1)

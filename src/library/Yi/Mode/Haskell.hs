{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Yi.Mode.Haskell
-- License     :  GPL-2
-- Maintainer  :  yi-devel@googlegroups.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Collection of 'Mode's for working with Haskell.

module Yi.Mode.Haskell
  (
   -- * Modes
   haskellAbstract,
   cleverMode,
   preciseMode,
   literateMode,
   fastMode,

   -- * IO-level operations
   ghciGet,
   ghciSend,
   ghciLoadBuffer,
   ghciInferType,
   ghciSetProcessName,
   ghciSetProcessArgs
  ) where

import           Prelude                   hiding (all, concatMap, elem, error, notElem, exp)

import           Control.Applicative       (Applicative ((*>)), (<$>))
import           Control.Lens              ((&), (.~), (^.))
import           Control.Monad             (unless, void, when)
import           Data.Binary               (Binary)
import           Data.Default              (Default)
import           Data.Foldable             (Foldable, all, concatMap, elem, forM_, notElem)
import           Data.Maybe                (isJust, listToMaybe)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T (any, concat, drop, pack, unpack, unwords)
import           Data.Typeable             (Typeable)
import           Text.Read                 (readMaybe)
import           Yi.Buffer
import           Yi.Core                   (sendToProcess)
import           Yi.Debug                  (error, trace)
import           Yi.Editor
import           Yi.File                   (fwriteE)
import qualified Yi.IncrementalParse       as IncrParser (State, scanner)
import           Yi.Keymap                 (YiM)
import           Yi.Lexer.Alex
import           Yi.Lexer.Haskell          as Haskell
import qualified Yi.Lexer.LiterateHaskell  as LiterateHaskell (HlState, alexScanToken, initState)
import           Yi.MiniBuffer             (noHint, withMinibufferFree, withMinibufferGen)
import qualified Yi.Mode.GHCi              as GHCi (ghciProcessArgs, ghciProcessName, spawnProcess)
import qualified Yi.Mode.Interactive       as Interactive (queryReply)
import           Yi.Modes                  (anyExtension, extensionOrContentsMatch)
import           Yi.Monad                  (gets)
import qualified Yi.Rope                   as R
import           Yi.String                 (fillText, showT)
import           Yi.Syntax                 (ExtHL (..), Scanner, skipScanner)
import qualified Yi.Syntax.Driver          as Driver (mkHighlighter)
import           Yi.Syntax.Haskell         as Hask
import           Yi.Syntax.Layout          (State)
import           Yi.Syntax.OnlineTree      as OnlineTree (Tree, manyToks)
import           Yi.Syntax.Paren           as Paren
import           Yi.Syntax.Strokes.Haskell as HS (getStrokes)
import           Yi.Syntax.Tree
import           Yi.Types                  (YiVariable)
import           Yi.Utils                  (groupBy')

-- | General ‘template’ for actual Haskell modes.
--
-- It applies over @extensions = ["hs", "x", "hsc", "hsinc"]@ which
-- may be a little questionable but for now Yi is mostly used by
-- Haskell hackers so it should be fine, at least for now.
haskellAbstract :: Mode (tree TT)
haskellAbstract = emptyMode
  & modeAppliesA .~ extensionOrContentsMatch extensions shebangPattern
  & modeNameA .~ "haskell"
  & modeToggleCommentSelectionA .~ Just (toggleCommentB "--")
  where extensions = ["hs", "x", "hsc", "hsinc"]
        shebangPattern = "^#![[:space:]]*/usr/bin/env[[:space:]]+runhaskell"

-- | "Clever" haskell mode, using the paren-matching syntax.
cleverMode :: Mode (Paren.Tree (Tok Haskell.Token))
cleverMode = haskellAbstract
  & modeIndentA .~ cleverAutoIndentHaskellB
  & modeGetStrokesA .~ strokesOfParenTree
  & modeHLA .~ mkParenModeHL (skipScanner 50) haskellLexer
  & modeAdjustBlockA .~ adjustBlock
  & modePrettifyA .~ cleverPrettify . allToks

fastMode :: Mode (OnlineTree.Tree TT)
fastMode = haskellAbstract
  & modeNameA .~ "fast haskell"
  & modeHLA .~ mkOnlineModeHL haskellLexer
  & modeGetStrokesA .~ tokenBasedStrokes Paren.tokenToStroke

literateMode :: Mode (Paren.Tree TT)
literateMode = haskellAbstract
  & modeNameA .~ "literate haskell"
  & modeAppliesA .~ anyExtension ["lhs"]
  & modeHLA .~ mkParenModeHL id literateHaskellLexer
  & modeGetStrokesA .~ strokesOfParenTree
    -- FIXME I think that 'begin' should not be ignored
  & modeAdjustBlockA .~ adjustBlock
  & modeIndentA .~ cleverAutoIndentHaskellB
  & modePrettifyA .~ cleverPrettify . allToks

-- | Experimental Haskell mode, using a rather precise parser for the syntax.
preciseMode :: Mode (Hask.Tree TT)
preciseMode = haskellAbstract
  & modeNameA .~ "precise haskell"
  & modeIndentA .~ cleverAutoIndentHaskellC
  & modeGetStrokesA .~ (\ast point begin end -> HS.getStrokes point begin end ast)
  & modeHLA .~ mkHaskModeHL haskellLexer
  & modePrettifyA .~ cleverPrettify . allToks
--
strokesOfParenTree :: Paren.Tree TT -> Point -> Point -> Point -> [Stroke]
strokesOfParenTree t p b e = Paren.getStrokes p b e t

type CharToTTScanner s = CharScanner -> Scanner (AlexState s) TT

mkParenModeHL :: (IsTree tree, Show state)
              => (Scanner
                  (IncrParser.State (State Token lexState) TT (Paren.Tree TT))
                  (Paren.Tree TT)
                  -> Scanner state (tree (Tok tt)))
              -> CharToTTScanner lexState
              -> ExtHL (tree (Tok tt))
mkParenModeHL f l = ExtHL $ Driver.mkHighlighter scnr
  where
    scnr = f . IncrParser.scanner Paren.parse . Paren.indentScanner . l

mkHaskModeHL :: Show st => CharToTTScanner st -> ExtHL (Exp (Tok Token))
mkHaskModeHL l = ExtHL $ Driver.mkHighlighter scnr
  where
    scnr = IncrParser.scanner Hask.parse . Hask.indentScanner . l

mkOnlineModeHL :: Show st => (CharScanner -> Scanner st (Tok tt))
               -> ExtHL (OnlineTree.Tree (Tok tt))
mkOnlineModeHL l = ExtHL $ Driver.mkHighlighter scnr
  where
    scnr = IncrParser.scanner OnlineTree.manyToks . l

haskellLexer :: CharScanner -> Scanner (AlexState Haskell.HlState) TT
haskellLexer = lexScanner (commonLexer Haskell.alexScanToken Haskell.initState)

literateHaskellLexer :: CharScanner -> Scanner (AlexState LiterateHaskell.HlState) TT
literateHaskellLexer = lexScanner (commonLexer LiterateHaskell.alexScanToken LiterateHaskell.initState)

adjustBlock :: Paren.Tree (Tok Token) -> Int -> BufferM ()
adjustBlock e len = do
  p <- pointB
  l <- curLn
  let t = Paren.getIndentingSubtree e p l
  case t of
    Nothing -> return ()
    Just it -> savingExcursionB $ do
      let (_startOfs, height) = Paren.getSubtreeSpan it
      col <- curCol
      forM_ [1..height] $ const $ do
        lineDown
        indent <- indentOfB =<< readLnB
        -- it might be that we have 1st column comments in the block,
        -- which should not be changed.
        when (indent > col) $
         if len >= 0
          then do
           insertN $ R.replicateChar len ' '
           leftN len
          else deleteN (negate len)

-- | Returns true if the token should be indented to look as "inside"
-- the group.
insideGroup :: Token -> Bool
insideGroup (Special c) = T.any (== c) "',;})]"
insideGroup _ = True

-- | Helper method for taking information needed for both Haskell auto-indenters:
indentInfoB :: BufferM (Int, Int, Int, Point, Point)
indentInfoB = do
  indentLevel    <- shiftWidth <$> indentSettingsB
  previousIndent <- indentOfB =<< getNextNonBlankLineB Backward
  nextIndent     <- indentOfB =<< getNextNonBlankLineB Forward
  solPnt         <- pointAt moveToSol
  eolPnt         <- pointAt moveToEol
  return (indentLevel, previousIndent, nextIndent, solPnt, eolPnt)

cleverAutoIndentHaskellB :: Paren.Tree TT -> IndentBehaviour -> BufferM ()
cleverAutoIndentHaskellB e behaviour = do
  (indentLevel, previousIndent, nextIndent, solPnt, eolPnt) <- indentInfoB
  let onThisLine ofs = ofs >= solPnt && ofs <= eolPnt
      firstTokNotOnLine = listToMaybe .
                              filter (not . onThisLine . posnOfs . tokPosn) .
                              filter (not . isErrorTok . tokT) . concatMap allToks
  let stopsOf :: [Paren.Tree TT] -> [Int]
      stopsOf (g@(Paren.Paren open ctnt close):ts')
          | isErrorTok (tokT close) || getLastOffset g >= solPnt
              = [groupIndent open ctnt]  -- stop here: we want to be "inside" that group.
          | otherwise = stopsOf ts' -- this group is closed before this line; just skip it.
      stopsOf (Paren.Atom (Tok {tokT = t}):_) | startsLayout t = [nextIndent, previousIndent + indentLevel]
        -- of; where; etc. we want to start the block here.
        -- Also use the next line's indent:
        -- maybe we are putting a new 1st statement in the block here.
      stopsOf (Paren.Atom _:ts) = stopsOf ts
         -- any random part of expression, we ignore it.
      stopsOf (t@(Paren.Block _):ts) = shiftBlock + maybe 0 (posnCol . tokPosn) (getFirstElement t) : stopsOf ts
      stopsOf (_:ts) = stopsOf ts
      stopsOf [] = []
      firstTokOnLine = fmap tokT $ listToMaybe $
          dropWhile ((solPnt >) . tokBegin) $
          takeWhile ((eolPnt >) . tokBegin) $ -- for laziness.
          filter (not . isErrorTok . tokT) $ allToks e
      shiftBlock = case firstTokOnLine of
        Just (Reserved t) | t `elem` [Where, Deriving] -> indentLevel
        Just (ReservedOp Haskell.Pipe) -> indentLevel
        Just (ReservedOp Haskell.Equal) -> indentLevel
        _ -> 0
      deepInGroup = maybe True insideGroup firstTokOnLine
      groupIndent (Tok {tokT = Special openChar, tokPosn = Posn _ _ openCol}) ctnt
          | deepInGroup = case firstTokNotOnLine ctnt of
              -- examine the first token of the group (but not on the line we are indenting!)
              Nothing -> openCol + nominalIndent openChar -- no such token: indent normally.
              Just t -> posnCol . tokPosn $ t -- indent along that other token
          | otherwise = openCol
      groupIndent (Tok {}) _ = error "unable to indent code"
  case getLastPath [e] solPnt of
    Nothing -> return ()
    Just path -> let stops = stopsOf path
                 in trace ("Stops = " <> showT stops) $
                    trace ("firstTokOnLine = " <> showT firstTokOnLine) $
                    cycleIndentsB behaviour stops

cleverAutoIndentHaskellC :: Exp TT -> IndentBehaviour -> BufferM ()
cleverAutoIndentHaskellC e behaviour = do
  (indentLevel, previousIndent, nextIndent, solPnt, eolPnt) <- indentInfoB
  let onThisLine ofs = ofs >= solPnt && ofs <= eolPnt
      firstTokNotOnLine = listToMaybe .
                              filter (not . onThisLine . posnOfs . tokPosn) .
                              filter (not . isErrorTok . tokT) . concatMap allToks
  let stopsOf :: [Hask.Exp TT] -> [Int]
      stopsOf (g@(Hask.Paren (Hask.PAtom open _) ctnt (Hask.PAtom close _)):ts)
          | isErrorTok (tokT close) || getLastOffset g >= solPnt
              = [groupIndent open ctnt]
            -- stop here: we want to be "inside" that group.
          | otherwise = stopsOf ts
           -- this group is closed before this line; just skip it.
      stopsOf (Hask.PAtom (Tok {tokT = t}) _:_) | startsLayout t || (t == ReservedOp Equal)
          = [nextIndent, previousIndent + indentLevel]
        -- of; where; etc. ends the previous line. We want to start the block here.
        -- Also use the next line's indent:
        -- maybe we are putting a new 1st statement in the block here.
      stopsOf (l@(Hask.PLet _ (Hask.Block _) _):ts') = [colOf' l | lineStartsWith (Reserved Haskell.In)] <> stopsOf ts'
                                                       -- offer to align with let only if this is an "in"
      stopsOf (t@(Hask.Block _):ts') = (shiftBlock + colOf' t) : stopsOf ts'
                                       -- offer add another statement in the block
      stopsOf (Hask.PGuard' (PAtom pipe _) _ _:ts') = [tokCol pipe | lineStartsWith (ReservedOp Haskell.Pipe)] <> stopsOf ts'
                                                                 -- offer to align against another guard
      stopsOf (d@(Hask.PData {}):ts') = colOf' d + indentLevel
                                           : stopsOf ts' --FIXME!
      stopsOf (Hask.RHS (Hask.PAtom{}) exp:ts')
          = [case firstTokOnLine of
              Just (Operator op') -> opLength op' (colOf' exp) -- Usually operators are aligned against the '=' sign
              -- case of an operator should check so that value always is at least 1
              _ -> colOf' exp | lineIsExpression ] <> stopsOf ts'
                   -- offer to continue the RHS if this looks like an expression.
      stopsOf [] = [0] -- maybe it's new declaration in the module
      stopsOf (_:ts) = stopsOf ts -- by default, there is no reason to indent against an expression.
       -- calculate indentation of operator (must be at least 1 to be valid)
      opLength ts' r = let l = r - (length ts' + 1) -- I find this dubious...
                       in  if l > 0 then l else 1

      lineStartsWith tok = firstTokOnLine == Just tok
      lineIsExpression   = all (`notElem` [ReservedOp Haskell.Pipe, ReservedOp Haskell.Equal, ReservedOp RightArrow]) toksOnLine
                           && not (lineStartsWith (Reserved Haskell.In))
      -- TODO: check the tree instead of guessing by looking at tokens
      firstTokOnLine = listToMaybe toksOnLine
      toksOnLine = fmap tokT $
          dropWhile ((solPnt >) . tokBegin) $
          takeWhile ((eolPnt >) . tokBegin) $ -- for laziness.
          filter (not . isErrorTok . tokT) $ allToks e
      shiftBlock = case firstTokOnLine of
        Just (Reserved t) | t `elem` [Where, Deriving] -> indentLevel
        Just (ReservedOp Haskell.Pipe) -> indentLevel
        Just (ReservedOp Haskell.Equal) -> indentLevel
        _ -> 0
      deepInGroup = maybe True insideGroup firstTokOnLine
      groupIndent (Tok {tokT = Special openChar, tokPosn = Posn _ _ openCol}) ctnt
          | deepInGroup = case firstTokNotOnLine ctnt of
              -- examine the first token of the group
              -- (but not on the line we are indenting!)
              Nothing -> openCol + nominalIndent openChar
              -- no such token: indent normally.
              Just t -> posnCol . tokPosn $ t -- indent along that other token
          | otherwise = openCol
      groupIndent (Tok{}) _ = error "unable to indent code"
  case getLastPath [e] solPnt of
    Nothing -> return ()
    Just path ->let stops = stopsOf path
                in trace ("Path = " <> showT path) $
                   trace ("Stops = " <> showT stops) $
                   trace ("Previous indent = " <> showT previousIndent) $
                   trace ("Next indent = " <> showT nextIndent) $
                   trace ("firstTokOnLine = " <> showT firstTokOnLine) $
                   cycleIndentsB behaviour stops

colOf' :: Foldable t => t TT -> Int
colOf' = maybe 0 tokCol . getFirstElement

tokCol :: Tok t -> Int
tokCol = posnCol . tokPosn


nominalIndent :: Char -> Int
nominalIndent '{' = 2
nominalIndent _ = 1

tokText :: Tok t -> BufferM R.YiString
tokText = readRegionB . tokRegion

tokRegion :: Tok t -> Region
tokRegion t = mkRegion (tokBegin t) (tokEnd t)

isLineComment :: TT -> Bool
isLineComment = (Just Haskell.Line ==) . tokTyp . tokT

contiguous :: Tok t -> Tok t -> Bool
contiguous a b = lb - la <= 1
    where [la,lb] = fmap (posnLine . tokPosn) [a,b]

coalesce :: Tok Token -> Tok Token -> Bool
coalesce a b = isLineComment a && isLineComment b && contiguous a b

cleverPrettify :: [TT] -> BufferM ()
cleverPrettify toks = do
  pnt <- pointB
  let groups = groupBy' coalesce toks
      isCommentGroup g = tokTyp (tokT $ head g) `elem` fmap Just [Haskell.Line]
      thisCommentGroup = listToMaybe $ dropWhile ((pnt >) . tokEnd . last) $ filter isCommentGroup groups
                         -- FIXME: laziness
  case thisCommentGroup of
    Nothing -> return ()
    Just g -> do
      text <- T.unwords . fmap (T.drop 2 . R.toText) <$> mapM tokText g
      let region = mkRegion (tokBegin . head $ g) (tokEnd . last $ g)
          mkGrp = const . R.unlines $ R.append "-- " <$> fillText 80 (R.fromText text)
      modifyRegionB mkGrp region

tokTyp :: Token -> Maybe Haskell.CommentType
tokTyp (Comment t) = Just t
tokTyp _ = Nothing

-- TODO: export or remove
-- -- Keyword-based auto-indenter for haskell.
-- autoIndentHaskellB :: IndentBehaviour -> BufferM ()
-- autoIndentHaskellB =
--   autoIndentWithKeywordsB [ "if"
--                           , "then"
--                           , "else"
--                           , "|"
--                           , "->"
--                           , "case" -- hmm
--                           , "in"
--                           -- Note tempted by having '=' in here that would
--                           -- potentially work well for 'data' declarations
--                           -- but I think '=' is so common in other places
--                           -- that it would introduce many spurious/annoying
--                           -- hints.
--                           ]
--                           [ "where"
--                           , "let"
--                           , "do"
--                           , "mdo"
--                           , "{-"
--                           , "{-|"
--                           , "--"
--                           ]
--
---------------------------
-- * Interaction with GHCi

-- | Variable storing the possibe buffer reference where GHCi is
-- currently running.
newtype GhciBuffer = GhciBuffer {_ghciBuffer :: Maybe BufferRef}
    deriving (Default, Typeable, Binary)

instance YiVariable GhciBuffer

-- | Start GHCi in a buffer
ghci :: YiM BufferRef
ghci = do
  g <- getEditorDyn
  b <- GHCi.spawnProcess (g ^. GHCi.ghciProcessName) (g ^. GHCi.ghciProcessArgs)
  withEditor . putEditorDyn . GhciBuffer $ Just b
  return b

-- | Return GHCi's buffer; create it if necessary.
-- Show it in another window.
ghciGet :: YiM BufferRef
ghciGet = withOtherWindow $ do
    GhciBuffer mb <- withEditor getEditorDyn
    case mb of
        Nothing -> ghci
        Just b -> do
            stillExists <- isJust <$> findBuffer b
            if stillExists
                then do withEditor $ switchToBufferE b
                        return b
                else ghci

-- | Send a command to GHCi
ghciSend :: String -> YiM ()
ghciSend cmd = do
    b <- ghciGet
    withGivenBuffer b botB
    sendToProcess b (cmd <> "\n")

-- | Load current buffer in GHCi
ghciLoadBuffer :: YiM ()
ghciLoadBuffer = do
    void fwriteE
    f <- withCurrentBuffer (gets file)
    case f of
      Nothing -> error "Couldn't get buffer filename in ghciLoadBuffer"
      Just filename -> ghciSend $ ":load " <> show filename

-- Tells ghci to infer the type of the identifier at point. Doesn't
-- check for errors (yet)
ghciInferType :: YiM ()
ghciInferType = do
    nm <- withCurrentBuffer (readUnitB unitWord)
    unless (R.null nm) $
      withMinibufferGen (R.toText nm) noHint "Insert type of which identifier?"
      return (const $ return ()) (ghciInferTypeOf . R.fromText)

ghciInferTypeOf :: R.YiString -> YiM ()
ghciInferTypeOf nm = do
    buf <- ghciGet
    result <- Interactive.queryReply buf (":t " <> R.toString nm)
    let successful = (not . R.null) nm && nm == result
    when successful . withCurrentBuffer $
      moveToSol *> insertB '\n' *> leftB
      *> insertN result *> rightB

ghciSetProcessName :: YiM ()
ghciSetProcessName = do
  g <- getEditorDyn
  let nm = g ^. GHCi.ghciProcessName
      prompt = T.concat [ "Command to call for GHCi, currently ‘"
                        , T.pack nm, "’: " ]
  withMinibufferFree prompt $ \s ->
    putEditorDyn $ g & GHCi.ghciProcessName .~ T.unpack s

ghciSetProcessArgs :: YiM ()
ghciSetProcessArgs = do
  g <- getEditorDyn
  let nm = g ^. GHCi.ghciProcessName
      args = g ^. GHCi.ghciProcessArgs
      prompt = T.unwords [ "List of args to call "
                         , T.pack nm
                         , "with, currently"
                         , T.pack $ show args
                         , ":"
                         ]
  withMinibufferFree prompt $ \arg ->
    case readMaybe $ T.unpack arg of
      Nothing -> printMsg "Could not parse as [String], keep old args."
      Just arg' -> putEditorDyn $ g & GHCi.ghciProcessArgs .~ arg'

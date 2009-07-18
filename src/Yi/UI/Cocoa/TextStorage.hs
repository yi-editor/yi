{-# LANGUAGE TemplateHaskell, EmptyDataDecls, MultiParamTypeClasses,
             FlexibleInstances, TypeSynonymInstances,
             DeriveDataTypeable, Rank2Types #-}
--
-- Copyright (c) 2008 Gustav Munkby
--

-- | An implementation of NSTextStorage that uses Yi's FBuffer as
-- the backing store.

module Yi.UI.Cocoa.TextStorage
  ( TextStorage
  , initializeClass_TextStorage
  , newTextStorage
  , setTextStorageBuffer
  , visibleRangeChanged
  ) where

import Prelude ()
import Yi.Editor (currentRegex, emptyEditor, Editor)
import Yi.Prelude
import Yi.Buffer
import Yi.Style
import Yi.UI.Cocoa.Utils
import Yi.UI.Utils
import Yi.Window

import Control.Arrow

import Data.Char
import Data.Maybe
import qualified Data.Rope as R
import qualified Data.Map as M
import qualified Data.List as L

import Foreign hiding (new)
import Foreign.C

-- Specify Cocoa imports explicitly, to avoid name-clashes.
-- Since the number of functions recognized by HOC varies
-- between revisions, this seems like the safest choice.
import HOC
import Foundation (
  Unichar,NSString,NSStringClass,NSDictionary,NSRange(..),NSRangePointer,
  NSStringMetaClass,toNSString,NSCharacterSet,_NSCharacterSet,invertedSet,
  _NSMutableCharacterSet,formUnionWithCharacterSet,newlineCharacterSet,
  length,attributeAtIndexEffectiveRange,attributesAtIndexEffectiveRange,
  attributesAtIndexLongestEffectiveRangeInRange,nsMaxRange,
  rangeOfCharacterFromSetOptionsRange,nsLiteralSearch,
  beginEditing,endEditing,setAttributesRange,haskellString,
  NSMutableDictionary,_NSMutableDictionary,dictionaryWithDictionary,
  objectForKey,setObjectForKey,substringWithRange,copy,
  attributeAtIndexLongestEffectiveRangeInRange,
  addAttributeValueRange,addAttributesRange)
import AppKit (
  NSTextStorage,NSTextStorageClass,string,fixesAttributesLazily,
  NSTextStorageMetaClass,NSFontClass,NSFont,coveredCharacterSet,
  _NSCursor,_NSFont,replaceCharactersInRangeWithString,
  _NSParagraphStyle,defaultParagraphStyle,ibeamCursor,_NSTextStorage,
  editedRangeChangeInLength,nsTextStorageEditedAttributes,
  nsTextStorageEditedCharacters,userFixedPitchFontOfSize)

-- Unfortunately, my version of hoc does not handle typedefs correctly,
-- and thus misses every selector that uses the "unichar" type, even
-- though it has introduced a type alias for it...
$(declareRenamedSelector "characterAtIndex:" "characterAtIndex" [t| CUInt -> IO Unichar |])
instance Has_characterAtIndex (NSString a)
$(declareRenamedSelector "getCharacters:range:" "getCharactersRange" [t| Ptr Unichar -> NSRange -> IO () |])
instance Has_getCharactersRange (NSString a)

-- A (hidden) utility method for looking up font substitutions (used by WebKit)
$(declareRenamedSelector "findFontLike:forString:withRange:inLanguage:" "findFontLikeForStringWithRangeInLanguage" [t| forall t1 t2 t3. NSFont t1 -> NSString t2 -> NSRange -> NSString t3 -> IO (NSFont ()) |])
instance Has_findFontLikeForStringWithRangeInLanguage (NSFontClass a)
_silence :: ImpType_findFontLikeForStringWithRangeInLanguage x y
_silence = undefined

-- | SplitRope provides means for tracking the position
--   of Cocoa reads from the underlying rope...
data SplitRope = SplitRope
  R.Rope -- Cocoa has moved beyond this portion
  R.Rope -- Cocoa is currently accessing this part
  Int -- From this offset...
  [Unichar] -- And in this format... =)
  [Unichar] -- But we keep the whole as a backup

-- | Create a new SplitRope, and initialize the encoded portion appropriately.
mkSplitRope :: R.Rope -> R.Rope -> SplitRope
mkSplitRope done next = SplitRope done next 0 acs acs
  where acs = concatMap (encodeUTF16 . fromEnum) (R.toString next)

-- | Get the length of the whole SplitRope.
sLength :: SplitRope -> Int
sLength (SplitRope done next _ _ _) = R.length done + R.length next

-- | Ensure that the specified position is in the first chunk of
--   the ``next'' rope.
sSplitAtChunkBefore :: Int -> SplitRope -> SplitRope
sSplitAtChunkBefore n s@(SplitRope done next _ _ _)
  | n < R.length done = mkSplitRope done' (R.append renext next)
  | R.null redone     = s
  | otherwise         = mkSplitRope (R.append done redone) next'
  where
    (done', renext) = R.splitAtChunkBefore n done
    (redone, next') = R.splitAtChunkBefore (n - R.length done) next

sSplitAt :: Int -> SplitRope -> SplitRope
sSplitAt n s = SplitRope done next n' (if n' >= off then L.drop (n' - off) cs else L.drop n' acs) acs
  where
    n' = n - R.length done
    SplitRope done next off cs acs = sSplitAtChunkBefore n s

encodeUTF16 :: Int -> [Unichar]
encodeUTF16 c
  | c < 0x10000 = [fromIntegral c]
  | otherwise   = let c' = c - 0x10000
                  in [0xd800 .|. (fromIntegral $ c' `shiftR` 10),
                      0xdc00 .|. (fromIntegral $ c' .&. 0x3ff)]

-- Introduce a NSString subclass that has a Data.Rope internally.
-- A NSString subclass needs to implement length and characterAtIndex,
-- and for performance reasons getCharactersRange.
-- This implementation is a hack just like the old bytestring one,
-- but less so as Rope uses character indices instead of byte indices.
-- In theory, this should work fine for all characters in the
-- unicode BMP. I am unsure as to what happens if any characters
-- outside of the BMP are used.
$(declareClass "YiRope" "NSString")
$(exportClass "YiRope" "yirope_" [
    InstanceVariable "str" [t| SplitRope |] [| mkSplitRope R.empty R.empty |]
  , InstanceMethod 'length -- '
  , InstanceMethod 'characterAtIndex -- '
  , InstanceMethod 'getCharactersRange -- '
  ])

yirope_length :: YiRope () -> IO CUInt
yirope_length slf = do
  -- logPutStrLn $ "Calling yirope_length (gah...)"
  slf #. _str >>= return . fromIntegral . sLength

yirope_characterAtIndex :: CUInt -> YiRope () -> IO Unichar
yirope_characterAtIndex i slf = do
  -- logPutStrLn $ "Calling yirope_characterAtIndex " ++ show i
  flip (modifyIVar _str) slf $ \s -> do
    s'@(SplitRope _ _ _ (c:_) _) <- return (sSplitAt (fromIntegral i) s)
    return (s', c)

yirope_getCharactersRange :: Ptr Unichar -> NSRange -> YiRope () -> IO ()
yirope_getCharactersRange p _r@(NSRange i l) slf = do
  -- logPutStrLn $ "Calling yirope_getCharactersRange " ++ show r
  flip (modifyIVar_ _str) slf $ \s -> do
    s'@(SplitRope _ _ _ cs _) <- return (sSplitAt (fromIntegral i) s)
    pokeArray p (L.take (fromIntegral l) cs)
    return s'

-- An implementation of NSTextStorage that uses Yi's FBuffer as
-- the backing store. An implementation must at least implement
-- a O(1) string method and attributesAtIndexEffectiveRange.
-- For performance reasons, attributeAtIndexEffectiveRange is
-- implemented to deal with specific properties such as font.

-- Judging by usage logs, the environment using the text storage
-- seem to rely on strings O(1) behavior and thus caching the
-- result seems like a good idea. In addition attributes are
-- queried for the same location multiple times, and thus caching
-- them as well also seems fruitful.

type PicStroke = (CUInt, Attributes)
type Picture = [PicStroke]

instance Show NSRange where
  show (NSRange i len) = "NSRange " ++ show i ++ " " ++ show len

emptyPicture :: (Picture,NSRange)
emptyPicture = ([],NSRange 0 0)

dropStrokesWhile :: (a -> Bool) -> [a] -> [a]
dropStrokesWhile f = helper
  where
    helper [] = []
    helper [_] = []
    helper ~(x:y:zs) = if f y then helper (y:zs) else (x:y:zs)

attributeIsCached :: CUInt -> [PicStroke] -> Bool
attributeIsCached _ [] = False
attributeIsCached i ~((j,_):_) = i >= j

-- | Extend the currently cached picture, so that it at least
--   covers the desired region. The resulting picture starts
--   at the location of the desired region, but might extend
--   further...
extendPicture :: CUInt -> (CUInt -> IO Picture) -> Picture -> IO Picture
extendPicture d f c =
  if attributeIsCached d c
    then return c
    else f d

type YiState = (Editor, FBuffer, Window, UIStyle, YiRope ())

$(declareClass "YiTextStorage" "NSTextStorage")
$(exportClass "YiTextStorage" "yts_" [
    InstanceVariable "yiState" [t| YiState |] [| error "Uninitialized" |]
  , InstanceVariable "dictionaryCache" [t| M.Map Attributes (NSDictionary ()) |] [| M.empty |]
  , InstanceVariable "pictureCache" [t| (Picture, NSRange) |] [| emptyPicture |]
  , InstanceVariable "attributeCache" [t| [PicStroke] |] [| [] |]
  , InstanceVariable "covered" [t| (NSCharacterSet (), NSCharacterSet ()) |] [| error "Uninitialized" |]
  , InstanceMethod 'string -- '
  , InstanceMethod 'fixesAttributesLazily -- '
  , InstanceMethod 'attributeAtIndexEffectiveRange -- '
  , InstanceMethod 'attributeAtIndexLongestEffectiveRangeInRange
  , InstanceMethod 'attributesAtIndexEffectiveRange -- '
  , InstanceMethod 'attributesAtIndexLongestEffectiveRangeInRange
  , InstanceMethod 'replaceCharactersInRangeWithString -- '
  , InstanceMethod 'setAttributesRange     -- Disallow changing attributes
  , InstanceMethod 'addAttributesRange     -- optimized to avoid needless work
  , InstanceMethod 'addAttributeValueRange -- ...
  , InstanceMethod 'length -- '
  ])

_editor :: YiTextStorage () -> IO Editor
_buffer :: YiTextStorage () -> IO FBuffer
_window :: YiTextStorage () -> IO Window
_uiStyle :: YiTextStorage () -> IO UIStyle
_stringCache :: YiTextStorage () -> IO (YiRope ())
_editor      o = (\ (x,_,_,_,_) -> x) <$>  o #. _yiState
_buffer      o = (\ (_,x,_,_,_) -> x) <$>  o #. _yiState
_window      o = (\ (_,_,x,_,_) -> x) <$>  o #. _yiState
_uiStyle     o = (\ (_,_,_,x,_) -> x) <$>  o #. _yiState
_stringCache o = (\ (_,_,_,_,x) -> x) <$>  o #. _yiState

yts_length :: YiTextStorage () -> IO CUInt
yts_length slf = do
  -- logPutStrLn "Calling yts_length "
  slf # _stringCache >>= length

yts_string :: YiTextStorage () -> IO (NSString ())
yts_string slf = castObject <$> slf # _stringCache

yts_fixesAttributesLazily :: YiTextStorage () -> IO Bool
yts_fixesAttributesLazily _ = return True

yts_attributesAtIndexEffectiveRange :: CUInt -> NSRangePointer -> YiTextStorage () -> IO (NSDictionary ())
yts_attributesAtIndexEffectiveRange i er slf = do
  (cache, _) <- slf #. _pictureCache
  if attributeIsCached i cache
    then returnEffectiveRange cache i er (NSRange i 12345678) slf
    else yts_attributesAtIndexLongestEffectiveRangeInRange i er (NSRange i 1) slf

yts_attributesAtIndexLongestEffectiveRangeInRange :: CUInt -> NSRangePointer -> NSRange -> YiTextStorage () -> IO (NSDictionary ())
yts_attributesAtIndexLongestEffectiveRangeInRange i er rl@(NSRange il _) slf = do
  (cache, prev_rl) <- slf #. _pictureCache
  -- Since we only cache the remaining part of the rl window, we must
  -- check to ensure that we do not re-read the window all the time...
  let use_i = if prev_rl == rl then i else il
  -- logPutStrLn $ "yts_attributesAtIndexLongestEffectiveRangeInRange " ++ show i ++ " " ++ show rl
  full <- extendPicture use_i (flip storagePicture slf) cache
  returnEffectiveRange full i er rl slf

returnEffectiveRange :: Picture -> CUInt -> NSRangePointer -> NSRange -> YiTextStorage () -> IO (NSDictionary ())
returnEffectiveRange full i er rl@(NSRange il ll) slf = do
  pic <- return $ dropStrokesWhile ((fromIntegral i >=) . fst) full
  slf # setIVar _pictureCache (pic, rl)
  case pic of
    (before,s):rest@((next,_):_) -> do
      let begin = max before il
      let len = min (il + ll - begin) (next - begin)
      let rng = NSRange begin len
      -- Keep a cache of seen styles... usually, there should not be to many
      -- TODO: Have one centralized cache instead of one per text storage...
      dict <- slf # cachedDictionaryFor s
      if (nsMaxRange rng == begin)
        then return dict
        else do
          str <- yts_string slf
          (covered, missing) <- slf #. _covered
          NSRange b2 _ <- str # rangeOfCharacterFromSetOptionsRange missing nsLiteralSearch rng
          if begin /= b2 -- First caracter is included in the font
            then do
              let corange = NSRange begin $ if b2 == 0x7fffffff then len else b2 - begin
              -- logPutStrLn $ "Normal " ++ show i ++ show rl ++ show corange
              when (er /= nullPtr) (poke er corange)
              when (rng /= corange) $
                slf # setIVar _pictureCache ((before,s):(nsMaxRange corange,s):rest, rl)
              return dict
            else do
              NSRange b3 _ <- str # rangeOfCharacterFromSetOptionsRange covered nsLiteralSearch rng
              let unrange = NSRange begin $ if b3 == 0x7fffffff then len else b3 - begin
              rep <- str # substringWithRange unrange >>= haskellString
              -- logPutStrLn $ "Fixing " ++ show unrange ++ ": " ++ show rep
              font <- castObject <$> dict # objectForKey (toNSString "NSFont") :: IO (NSFont ())
              font2 <- _NSFont # findFontLikeForStringWithRangeInLanguage font str unrange nil
              dict2 <- castObject <$> _NSMutableDictionary # dictionaryWithDictionary dict :: IO (NSMutableDictionary ())
              dict2 # setObjectForKey font2 (toNSString "NSFont")
              dict2 # setObjectForKey font (toNSString "NSOriginalFont")
              when (er /= nullPtr) (poke er unrange)
              when (rng /= unrange) $
                slf # setIVar _pictureCache ((before,s):(nsMaxRange unrange,s):rest, rl)
              castObject <$> return dict2
    _ -> error "Empty picture?"

cachedDictionaryFor :: Attributes -> YiTextStorage () -> IO (NSDictionary ())
cachedDictionaryFor s slf = do
  slf # modifyIVar _dictionaryCache (\dicts ->
    case M.lookup s dicts of
      Just dict -> return (dicts, dict)
      _ -> do
        dict <- convertAttributes s
        return (M.insert s dict dicts, dict))

simpleAttr :: String -> IO (Either String (ID ()))
simpleAttr "NSFont"           = Right <$> castObject <$> userFixedPitchFontOfSize 0 _NSFont
simpleAttr "NSGlyphInfo"      = Right <$> return nil
simpleAttr "NSAttachment"     = Right <$> return nil
simpleAttr "NSCursor"         = Right <$> castObject <$> ibeamCursor _NSCursor
simpleAttr "NSToolTip"        = Right <$> return nil
simpleAttr "NSLanguage"       = Right <$> return nil
simpleAttr "NSLink"           = Right <$> return nil
-- TODO: Adjust line break property...
simpleAttr "NSParagraphStyle" = Right <$> castObject <$> defaultParagraphStyle _NSParagraphStyle
simpleAttr attr               = Left <$> return attr

yts_attributeAtIndexLongestEffectiveRangeInRange :: NSString t -> CUInt -> NSRangePointer -> NSRange -> YiTextStorage () -> IO (ID ())
yts_attributeAtIndexLongestEffectiveRangeInRange attr i er rn slf = do
  sres <- simpleAttr =<< haskellString attr
  case sres of
    Right res -> safePoke er rn >> return res
    Left "NSBackgroundColor" -> do
      ~((s,a):xs) <- onlyBg <$> L.takeWhile ((<= nsMaxRange rn).fst) <$> slf # storagePicture i
      safePoke er (NSRange s ((if null xs then nsMaxRange rn else fst (head xs)) - s))
      castObject <$> getColor False (background a)
    Left _attr' -> do
      -- logPutStrLn $ "Unoptimized yts_attributeAtIndexLongestEffectiveRangeInRange " ++ attr' ++ " at " ++ show i
      super slf # attributeAtIndexLongestEffectiveRangeInRange attr i er rn

yts_attributeAtIndexEffectiveRange :: forall t. NSString t -> CUInt -> NSRangePointer -> YiTextStorage () -> IO (ID ())
yts_attributeAtIndexEffectiveRange attr i er slf = do
  sres <- simpleAttr =<< haskellString attr
  case sres of
    Right res -> slf # length >>= safePoke er . NSRange 0 >> return res
    Left "NSBackgroundColor" -> do
      len <- slf # length
      slf # yts_attributeAtIndexLongestEffectiveRangeInRange attr i er (NSRange i (min 100 (len - i)))
    Left _attr' -> do
      -- TODO: Optimize the other queries as well (if needed)
      -- logPutStrLn $ "Unoptimized yts_attributeAtIndexEffectiveRange " ++ attr' ++ " at " ++ show i
      super slf # attributeAtIndexEffectiveRange attr i er

-- These methods are used to modify the contents of the NSTextStorage.
-- We do not allow direct updates of the contents this way, though.
yts_replaceCharactersInRangeWithString :: forall t. NSRange -> NSString t -> YiTextStorage () -> IO ()
yts_replaceCharactersInRangeWithString _ _ _ = return ()
yts_setAttributesRange :: forall t. NSDictionary t -> NSRange -> YiTextStorage () -> IO ()
yts_setAttributesRange _ _ _ = return ()
yts_addAttributesRange :: NSDictionary t -> NSRange -> YiTextStorage () -> IO ()
yts_addAttributesRange _ _ _ = return ()
yts_addAttributeValueRange :: NSString t -> ID () -> NSRange -> YiTextStorage () -> IO ()
yts_addAttributeValueRange _ _ _ _ = return ()

-- | Remove element x_(i+1) if f(x_i,x_(i+1)) is true
filter2 :: (a -> a -> Bool) -> [a] -> [a]
filter2 _f [] = []
filter2 _f [x] = [x]
filter2 f (x1:x2:xs) =
  if f x1 x2 then filter2 f (x1:xs) else x1 : filter2 f (x2:xs)

-- | Keep only the background information
onlyBg :: [PicStroke] -> [PicStroke]
onlyBg = filter2 ((==) `on` (background . snd))

-- | A version of poke that does nothing if p is null.
safePoke :: (Storable a) => Ptr a -> a -> IO ()
safePoke p x = when (p /= nullPtr) (poke p x)

-- | Execute strokeRangesB on the buffer, and update the buffer
--   so that we keep around cached syntax information...
--   We assume that the incoming region provide character-indices,
--   and we need to find out the corresponding byte-indices
storagePicture :: CUInt -> YiTextStorage () -> IO Picture
storagePicture i slf = do
  (ed, buf, win, sty, _) <- slf #. _yiState
  -- logPutStrLn $ "storagePicture " ++ show i
  return $ bufferPicture ed sty buf win i

bufferPicture :: Editor -> UIStyle -> FBuffer -> Window -> CUInt -> Picture
bufferPicture ed sty buf win i = fmap (first fromIntegral) $ fst $ runBuffer win buf $ do
  e <- sizeB
  (attributesPictureB sty (currentRegex ed) (mkRegion (fromIntegral i) e) [])

type TextStorage = YiTextStorage ()
initializeClass_TextStorage :: IO ()
initializeClass_TextStorage = do
  initializeClass_YiRope
  initializeClass_YiTextStorage

applyUpdate :: YiTextStorage () -> FBuffer -> Update -> IO ()
applyUpdate buf b (Insert p _ s) =
  buf # editedRangeChangeInLength nsTextStorageEditedCharacters
          (NSRange (fromIntegral p) 0) (fromIntegral $ R.length s)

applyUpdate buf b (Delete p _ s) =
  let len = R.length s in
  buf # editedRangeChangeInLength nsTextStorageEditedCharacters
          (NSRange (fromIntegral p) (fromIntegral len)) (fromIntegral (negate len))

newTextStorage :: UIStyle -> FBuffer -> Window -> IO TextStorage
newTextStorage sty b w = do
  buf <- new _YiTextStorage
  s <- new _YiRope
  s # setIVar _str (mkSplitRope R.empty (runBufferDummyWindow b (streamB Forward 0)))
  buf # setIVar _yiState (emptyEditor, b, w, sty, s)
  -- Determine the set of characters in the font.
  -- Always add newlines, since they are currently not rendered anyhow...
  allset <- new _NSMutableCharacterSet
  buf # setMonospaceFont >>= coveredCharacterSet >>= flip formUnionWithCharacterSet allset
  _NSCharacterSet # newlineCharacterSet >>= flip formUnionWithCharacterSet allset . castObject
  covset <- castObject <$> copy allset
  misset <- covset # invertedSet
  buf # setIVar _covered (covset, misset)
  return buf

setTextStorageBuffer :: Editor -> FBuffer -> TextStorage -> IO ()
setTextStorageBuffer ed buf storage = do
  storage # beginEditing
  flip (modifyIVar_ _yiState) storage $ \ (_,_,w,sty,s) -> do
    s # setIVar _str (mkSplitRope R.empty (runBufferDummyWindow buf (streamB Forward 0)))
    return (ed, buf, w, sty, s)
  when (not $ null $ getVal pendingUpdatesA buf) $ do
    mapM_ (applyUpdate storage buf) [u | TextUpdate u <- getVal pendingUpdatesA buf]
    storage # setIVar _pictureCache emptyPicture
  storage # endEditing

visibleRangeChanged :: NSRange -> TextStorage -> IO ()
visibleRangeChanged range storage = do
  storage # setIVar _pictureCache emptyPicture
  storage # editedRangeChangeInLength nsTextStorageEditedAttributes range 0

{-# LANGUAGE TemplateHaskell, EmptyDataDecls, MultiParamTypeClasses #-}
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

import Prelude (take, dropWhile)
import Yi.Prelude
import Yi.Buffer
import Yi.Style
import Yi.Syntax
import Yi.UI.Cocoa.Utils
import Yi.UI.Utils

import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L

import Foreign hiding (new)
import Foreign.C

import qualified Data.ByteString.Lazy as LB

-- Specify Cocoa imports explicitly, to avoid name-clashes.
-- Since the number of functions recognized by HOC varies
-- between revisions, this seems like the safest choice.
import HOC
import Foundation (
  Unichar,NSString,NSStringClass,NSDictionary,NSRange(..),NSRangePointer,
  length,attributeAtIndexEffectiveRange,attributesAtIndexEffectiveRange,
  attributesAtIndexLongestEffectiveRangeInRange,nsMaxRange,
  beginEditing,endEditing,setAttributesRange,haskellString,
  addAttributeValueRange,addAttributesRange)
import AppKit (
  NSTextStorage,NSTextStorageClass,string,fixesAttributesLazily,
  _NSCursor,_NSFont,replaceCharactersInRangeWithString,
  _NSParagraphStyle,castObject,defaultParagraphStyle,ibeamCursor,
  editedRangeChangeInLength,nsTextStorageEditedAttributes,
  nsTextStorageEditedCharacters,userFixedPitchFontOfSize)

-- Unfortunately, my version of hoc does not handle typedefs correctly,
-- and thus misses every selector that uses the "unichar" type, even
-- though it has introduced a type alias for it...
$(declareRenamedSelector "characterAtIndex:" "characterAtIndex" [t| CUInt -> IO Unichar |])
instance Has_characterAtIndex (NSString a)
$(declareRenamedSelector "getCharacters:range:" "getCharactersRange" [t| Ptr Unichar -> NSRange -> IO () |])
instance Has_getCharactersRange (NSString a)

-- Introduce a NSString subclass that has a lazy bytestring internally
-- A NSString subclass needs to implement length and characterAtIndex,
-- and for performance reasons getCharactersRange
-- The implementation here is a quick hack and I have no idea how it
-- works with anything except ASCII characters. Cocoa uses UTF16 to
-- store characters, and Yi uses UTF8, so supposedly some recoding
-- has to take place. For UTF8 is converted to Char's that are then
-- just dealt with as if they were in UTF16...

$(declareClass "YiLBString" "NSString")
$(exportClass "YiLBString" "yls_" [
    InstanceVariable "str" [t| LB.ByteString |] [| LB.empty |]
  , InstanceMethod 'length -- '
  , InstanceMethod 'characterAtIndex -- '
  , InstanceMethod 'getCharactersRange -- '
  ])

yls_length :: YiLBString () -> IO CUInt
yls_length slf = do
  -- logPutStrLn $ "Calling yls_length (gah...)"
  slf #. _str >>= return . fromIntegral . LB.length

-- TODO: The result type should be UTF16...
yls_characterAtIndex :: CUInt -> YiLBString () -> IO Unichar
yls_characterAtIndex i slf = do
  -- logPutStrLn $ "Calling yls_characterAtIndex " ++ show i
  slf #. _str >>= return . fromIntegral . flip LB.index (fromIntegral i)

-- TODO: Should get an array of characters in UTF16...
yls_getCharactersRange :: Ptr Unichar -> NSRange -> YiLBString () -> IO ()
yls_getCharactersRange p _r@(NSRange i l) slf = do
  -- logPutStrLn $ "Calling yls_getCharactersRange " ++ show r
  slf #. _str >>=
    pokeArray p .
    take (fromIntegral l) . -- TODO: Is l given in bytes or characters?
    fmap fromIntegral . -- TODO: UTF16 recode
    LB.unpack .
    LB.drop (fromIntegral i)


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

-- | Use this as the base length of computed stroke ranges
strokeRangeExtent :: Num t => t
strokeRangeExtent = 4000

type PicStroke = (Point, Attributes)
data Picture = Picture
  { picRegion :: Region
  , picStrokes :: [PicStroke]
  }

instance Show Picture where
  show (Picture r ss) = "{{"++show r ++": "++show (take 1 ss)++"@"++show (L.length ss)++"}}"

emptyPicture :: (Picture, NSRange)
emptyPicture = (Picture emptyRegion [], NSRange 0 0)

nullPicture :: Picture -> Bool
nullPicture = null . picStrokes -- Or empty region??

regionEnds :: Region -> (Point, Point)
regionEnds r = (regionStart r, regionEnd r)

dropStrokesWhile :: (PicStroke -> Bool) -> Picture -> Picture
dropStrokesWhile f pic = pic { picRegion = mkRegion nb pe, picStrokes = strokes }
  where 
    (pb, pe) = regionEnds $ picRegion pic
    (nb, strokes) = helper pb (picStrokes pic)
    helper :: Point -> [PicStroke] -> (Point, [PicStroke])
    helper p [] = (p,[])
    helper p ~(x:xs)
      | f x       = helper (fst x) xs
      | otherwise = (p, x:xs)

-- | Extend the currently cached picture, so that it at least
--   covers the desired region. The resulting picture starts
--   at the location of the desired region, but might extend
--   further...
extendPicture :: Region -> (Region -> IO Picture) -> Picture -> IO Picture
extendPicture desired ext cache = do
  -- All possible overlappings of desired and cache regions:
  -- dd   dd  ddd  ddd dddd dd  dd ddd  dd   dd  dd  ddd   dd <- desired
  --   cc  cc  ccc  cc  cc  ccc cc cc  cccc ccc cc  ccc  cc   <- cache
  --  A    B    E   B    A   N  N   E   N    N   A   E    A   <- Get All/Begin/End/None
  -- logPutStrLn $ "extendPicture " ++ show ((db `inRegion` (picRegion cache)), ((de `compare` cb) /= (de `compare` ce)))
  case (
    db `inRegion` (picRegion cache), -- Have start
    de `compare` cb /= de `compare` ce     -- Have end
    ) of 
    ( True,  True) -> return $ dropJunk cache
    ( True, False) -> append (dropJunk cache) <$> ext (mkExtentRegion ce de)
    (False,  True) -> flip append cache       <$> ext (mkRegion db cb)
    (False, False) -> ext (mkExtentRegion db de)
  -- ext (mkExtentRegion db de)
  where
    (db, de) = regionEnds desired
    (cb, ce) = regionEnds $ picRegion cache
    mkExtentRegion b e = mkSizeRegion b (max (b ~- e) strokeRangeExtent)
    dropJunk p = Picture -- Like dropStrokesWhile but always use db as starting point
      { picRegion = mkRegion db (regionEnd $ picRegion p) 
      , picStrokes = dropWhile ((db >=) . fst) (picStrokes p) 
      }
    append p1 p2 = Picture
      { picRegion = mkRegion (regionStart $ picRegion p1) (regionEnd $ picRegion p2)
      , picStrokes = picStrokes p1 ++ picStrokes p2
      }

mkRangeRegion :: NSRange -> Region
mkRangeRegion (NSRange i l) = mkSizeRegion (fromIntegral i) (fromIntegral l)

$(declareClass "YiTextStorage" "NSTextStorage")
$(exportClass "YiTextStorage" "yts_" [
    InstanceVariable "buffer" [t| Maybe FBuffer |] [| Nothing |]
  , InstanceVariable "uiStyle" [t| Maybe UIStyle |] [| Nothing |]
  , InstanceVariable "dictionaryCache" [t| M.Map Attributes (NSDictionary ()) |] [| M.empty |]
  , InstanceVariable "pictureCache" [t| (Picture, NSRange) |] [| emptyPicture |]
  , InstanceVariable "stringCache" [t| Maybe (YiLBString ()) |] [| Nothing |]
  , InstanceMethod 'string -- '
  , InstanceMethod 'fixesAttributesLazily -- '
  , InstanceMethod 'attributeAtIndexEffectiveRange -- '
  , InstanceMethod 'attributesAtIndexEffectiveRange -- '
  , InstanceMethod 'attributesAtIndexLongestEffectiveRangeInRange
  , InstanceMethod 'replaceCharactersInRangeWithString -- '
  , InstanceMethod 'setAttributesRange     -- Disallow changing attributes
  , InstanceMethod 'addAttributesRange     -- optimized to avoid needless work
  , InstanceMethod 'addAttributeValueRange -- ...
  , InstanceMethod 'length -- '
  ])

yts_length :: YiTextStorage () -> IO CUInt
yts_length slf = do
  -- logPutStrLn "Calling yts_length "
  (fromIntegral . flip runBufferDummyWindow sizeB . fromJust) <$> slf #. _buffer

yts_string :: YiTextStorage () -> IO (NSString ())
yts_string slf = castObject <$> fromJust <$> slf #. _stringCache

yts_fixesAttributesLazily :: YiTextStorage () -> IO Bool
yts_fixesAttributesLazily _ = return True


mkRegionRange :: Region -> NSRange
mkRegionRange r = NSRange (fromIntegral $ regionStart r) (fromIntegral $ regionSize r)


yts_attributesAtIndexEffectiveRange :: CUInt -> NSRangePointer -> YiTextStorage () -> IO (NSDictionary ())
yts_attributesAtIndexEffectiveRange i er slf = do
  (cache, _) <- slf #. _pictureCache
  if (fromIntegral i `inRegion` picRegion cache)
    then returnEffectiveRange cache i er (mkRegionRange $ picRegion cache) slf
    else yts_attributesAtIndexLongestEffectiveRangeInRange i er (NSRange i 1) slf

yts_attributesAtIndexLongestEffectiveRangeInRange :: CUInt -> NSRangePointer -> NSRange -> YiTextStorage () -> IO (NSDictionary ())
yts_attributesAtIndexLongestEffectiveRangeInRange i er rl slf = do
  (cache, prev_rl) <- slf #. _pictureCache
  -- Since we only cache the remaining part of the rl window, we must
  -- check to ensure that we do not re-read the window all the time...
  let use_rl = if prev_rl == rl then NSRange i (nsMaxRange rl) else rl
  -- logPutStrLn $ "yts_attributesAtIndexLongestEffectiveRangeInRange " ++ show i ++ " " ++ show rl
  full <- extendPicture (mkRangeRegion use_rl) (flip storagePicture slf) cache
  -- TODO: Only merge identical strokes when "needed"?
  returnEffectiveRange full i er rl slf

returnEffectiveRange :: Picture -> CUInt -> NSRangePointer -> NSRange -> YiTextStorage () -> IO (NSDictionary ())
returnEffectiveRange full i er rl slf = do
  pic <- return $ dropStrokesWhile ((fromIntegral i >=) . fst) full
  -- logPutStrLn $ "returnEffectiveRange " ++ show pic
  slf # setIVar _pictureCache (pic, rl)
  if nullPicture pic
    then error "Empty picture?"
    else do
      let begin = fromIntegral $ regionStart $ picRegion pic
      let (next,s) = head $ picStrokes pic
      let end = min (fromIntegral next) (nsMaxRange rl)
      safePoke er (NSRange begin (end - begin))
      dicts <- slf #. _dictionaryCache
      -- Keep a cache of seen styles... usually, there should not be to many
      -- TODO: Have one centralized cache instead of one per text storage...
      case M.lookup s dicts of
        Just dict -> return dict
        _ -> do
          dict <- convertAttributes s
          slf # setIVar _dictionaryCache (M.insert s dict dicts)
          return dict
  

yts_attributeAtIndexEffectiveRange :: forall t. NSString t -> CUInt -> NSRangePointer -> YiTextStorage () -> IO (ID ())
yts_attributeAtIndexEffectiveRange attr i er slf = do
  attr' <- haskellString attr
  case attr' of
    "NSFont" -> do
      safePokeFullRange >> castObject <$> userFixedPitchFontOfSize 0 _NSFont
    "NSGlyphInfo" -> do
      safePokeFullRange >> return nil
    "NSAttachment" -> do
      safePokeFullRange >> return nil
    "NSCursor" -> do
      safePokeFullRange >> castObject <$> ibeamCursor _NSCursor
    "NSToolTip" -> do
      safePokeFullRange >> return nil
    "NSLanguage" -> do
      safePokeFullRange >> return nil
    "NSLink" -> do
      safePokeFullRange >> return nil
    "NSParagraphStyle" -> do
      -- TODO: Adjust line break property...
      safePokeFullRange >> castObject <$> defaultParagraphStyle _NSParagraphStyle
    "NSBackgroundColor" -> do
      -- safePokeFullRange >> castObject <$> blackColor _NSColor
      len <- yts_length slf
      ~((s,a):_) <- onlyBg <$> picStrokes <$> slf # storagePicture (mkSizeRegion (fromIntegral i) strokeRangeExtent)
      safePoke er (NSRange i ((min len (fromIntegral s)) - i))
      castObject <$> getColor False (background a)
    _ -> do
      -- TODO: Optimize the other queries as well (if needed)
      -- logPutStrLn $ "Unoptimized yts_attributeAtIndexEffectiveRange " ++ attr' ++ " at " ++ show i
      super slf # attributeAtIndexEffectiveRange attr i er
  where
    safePokeFullRange = do
      Just b <- slf #. _buffer
      safePoke er (NSRange 0 (fromIntegral $ runBufferDummyWindow b sizeB))

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

-- | Remove element x_i if f(x_i,x_(i+1)) is true
filter2 :: (a -> a -> Bool) -> [a] -> [a]
filter2 _f [] = []
filter2 _f [x] = [x]
filter2 f (x1:x2:xs) =
  (if f x1 x2 then id else (x1:)) $ filter2 f (x2:xs)

-- | Keep only the background information
onlyBg :: [PicStroke] -> [PicStroke]
onlyBg = filter2 ((==) `on` (background . snd))

-- | Get a picture where each component (p,style) means apply the style
--   up until the given point p.
paintCocoaPicture :: UIStyle -> Point -> [PicStroke] -> [PicStroke]
paintCocoaPicture sty end = tail . stylesift (baseAttributes sty)
  where
    -- Turn a picture of use style from p into a picture of use style until p
    stylesift s [] = [(end,s)]
    stylesift s ((p,t):xs) = (p,s):(stylesift t xs)

-- | A version of poke that does nothing if p is null.
safePoke :: (Storable a) => Ptr a -> a -> IO ()
safePoke p x = when (p /= nullPtr) (poke p x)

-- | Execute strokeRangesB on the buffer, and update the buffer
--   so that we keep around cached syntax information...
storagePicture :: Region -> YiTextStorage () -> IO Picture
storagePicture r slf = do
  Just sty <- slf #. _uiStyle
  Just buf <- slf #. _buffer
  -- logPutStrLn $ "storagePicture " ++ show i
  return $ bufferPicture sty buf r

bufferPicture :: UIStyle -> FBuffer -> Region -> Picture
bufferPicture sty buf r = Picture
  { picRegion = r
  , picStrokes = paintCocoaPicture sty (regionEnd r) $
      runBufferDummyWindow buf (attributesPictureB sty Nothing r []) 
  }

type TextStorage = YiTextStorage ()
initializeClass_TextStorage :: IO ()
initializeClass_TextStorage = do
  initializeClass_YiLBString
  initializeClass_YiTextStorage

applyUpdate :: YiTextStorage () -> Update -> IO ()
applyUpdate buf (Insert p _ s) =
  buf # editedRangeChangeInLength nsTextStorageEditedCharacters
          (NSRange (fromIntegral p) 0) (fromIntegral $ LB.length s)

applyUpdate buf (Delete p _ s) =
  let len = LB.length s in
  buf # editedRangeChangeInLength nsTextStorageEditedCharacters
          (NSRange (fromIntegral p) (fromIntegral len)) (fromIntegral (negate len))

newTextStorage :: UIStyle -> FBuffer -> IO TextStorage
newTextStorage sty b = do
  buf <- new _YiTextStorage
  buf # setIVar _buffer (Just b)
  buf # setIVar _uiStyle (Just sty)
  s <- new _YiLBString
  s # setIVar _str (runBufferDummyWindow b (streamB Forward 0))
  buf # setIVar _stringCache (Just s)
  buf # setMonospaceFont
  return buf

setTextStorageBuffer :: FBuffer -> TextStorage -> IO ()
setTextStorageBuffer buf storage = do
  storage # beginEditing
  when (not $ null $ pendingUpdates buf) $ do
    mapM_ (applyUpdate storage) [u | TextUpdate u <- pendingUpdates buf]
    storage # setIVar _pictureCache emptyPicture
  storage # setIVar _buffer (Just buf)
  Just s <- storage #. _stringCache
  s # setIVar _str (runBufferDummyWindow buf (streamB Forward 0))
  storage # endEditing

visibleRangeChanged :: NSRange -> TextStorage -> IO ()
visibleRangeChanged range storage = do
  storage # editedRangeChangeInLength nsTextStorageEditedAttributes range 0
  storage # setIVar _pictureCache emptyPicture

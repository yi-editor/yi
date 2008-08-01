{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
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
  ) where

import Prelude (take, unzip, uncurry, dropWhile)
import Yi.Prelude
import Yi.Buffer
import Yi.Buffer.Implementation
import Yi.Style
import Yi.Syntax
import Yi.Window
import Yi.UI.Cocoa.Utils
import Yi.UI.Utils

import Data.Maybe
import qualified Data.Map as M

import Foreign hiding (new)
import Foreign.C

import qualified Data.ByteString.Lazy as LB

import Foundation hiding (minimum, new, init, null)
import AppKit hiding (concat, dictionary)

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
    InstanceVariable "string" [t| LB.ByteString |] [| LB.empty |]
  , InstanceMethod 'length -- '
  , InstanceMethod 'characterAtIndex -- '
  , InstanceMethod 'getCharactersRange -- '
  ])

yls_length :: YiLBString () -> IO CUInt
yls_length self = do
  -- logPutStrLn $ "Calling yls_length (gah...)"
  self #. _string >>= return . fromIntegral . LB.length

-- TODO: The result type should be UTF16...
yls_characterAtIndex :: CUInt -> YiLBString () -> IO Unichar
yls_characterAtIndex i self = do
  -- logPutStrLn $ "Calling yls_characterAtIndex " ++ show i
  self #. _string >>= return . fromIntegral . flip LB.index (fromIntegral i)

-- TODO: Should get an array of characters in UTF16...
yls_getCharactersRange :: Ptr Unichar -> NSRange -> YiLBString () -> IO ()
yls_getCharactersRange p r@(NSRange i l) self = do
  -- logPutStrLn $ "Calling yls_getCharactersRange " ++ show r
  self #. _string >>=
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

-- Unfortunately HOC does not export Instance Variables, and thus
-- we cannot provide a type signature for withCache
-- withCache :: (InstanceVariables st iv) => st -> IVar iv (Maybe vt) -> (vt -> Bool) -> IO vt -> IO vt

-- | Obtain the result of the action and cache that as the
--   instance variable ivar in self. Use existing cache if
--   a result is stored, and cond says it is still valid.
withCache self ivar cond action = do
  cache <- self #. ivar
  case cache of
    Just val | cond val -> return val
    otherwise -> do
      val <- action
      self # setIVar ivar (Just val)
      return val

-- | Use this as the base length of computed stroke ranges
strokeRangeExtent :: Point
strokeRangeExtent = 100

-- TODO: Investigate whether it is a good idea to cache
--       NSDictionary objects also in some fashion

$(declareClass "YiTextStorage" "NSTextStorage")
$(exportClass "YiTextStorage" "yts_" [
    InstanceVariable "buffer" [t| Maybe FBuffer |] [| Nothing |]
  , InstanceVariable "uiStyle" [t| Maybe UIStyle |] [| Nothing |]
  , InstanceVariable "attributesCache" [t| Maybe (NSRange, NSDictionary ()) |] [| Nothing |]
  , InstanceVariable "stringCache" [t| Maybe (NSString ()) |] [| Nothing |]
  , InstanceMethod 'string -- '
  , InstanceMethod 'attributeAtIndexEffectiveRange -- '
  , InstanceMethod 'attributesAtIndexEffectiveRange -- '
  , InstanceMethod 'replaceCharactersInRangeWithString -- '
  , InstanceMethod 'setAttributesRange -- '
  , InstanceMethod 'length -- '
  ])

yts_length :: YiTextStorage () -> IO CUInt
yts_length self = do
  -- logPutStrLn "Calling yts_length "
  (fromIntegral . flip runBufferDummyWindow sizeB . fromJust) <$> self #. _buffer

yts_string :: YiTextStorage () -> IO (NSString ())
yts_string self = do
  withCache self _stringCache (const True) $ do
    s <- autonew _YiLBString
    Just b <- self #. _buffer
    s # setIVar _string (runBufferDummyWindow b (streamB Forward 0))
    castObject <$> return s

yts_attributesAtIndexEffectiveRange :: CUInt -> NSRangePointer -> YiTextStorage () -> IO (NSDictionary ())
yts_attributesAtIndexEffectiveRange i er self = do
  (r,dict) <- withCache self _attributesCache (nsLocationInRange i . fst) $ do
    Just sty <- self #. _uiStyle
    strokes <- self # runStrokesAround i
    let (e,s) = minimalStyle sty (fromIntegral i + strokeRangeExtent) strokes
    let r = NSRange i (fromIntegral e - i)
    -- logPutStrLn $ "Calling yts_attributesAtIndexEffectiveRange " ++ show r
    (,) <$> pure r <*> convertStyle s
  safePoke er r
  return dict

yts_attributeAtIndexEffectiveRange :: forall t. NSString t -> CUInt -> NSRangePointer -> YiTextStorage () -> IO (ID ())
yts_attributeAtIndexEffectiveRange attr i er self = do
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
    "NSParagraphStyle" -> do
      -- TODO: Adjust line break property...
      safePokeFullRange >> castObject <$> defaultParagraphStyle _NSParagraphStyle
    "NSBackgroundColor" -> do
      Just sty <- self #. _uiStyle
      bg <- minimalAttr sty background <$> self # runStrokesAround i
      let (s, Background c) = fromMaybe (fromIntegral i + strokeRangeExtent, Background Default) bg
      safePoke er (NSRange i (fromIntegral s - i))
      castObject <$> getColor False c
    _ -> do
      -- TODO: Optimize the other queries as well (if needed)
      logPutStrLn $ "Unoptimized yts_attributeAtIndexEffectiveRange " ++ attr' ++ " at " ++ show i
      super self # attributeAtIndexEffectiveRange attr i er
  where
    safePokeFullRange = do
      Just b <- self #. _buffer
      safePoke er (NSRange 0 (fromIntegral $ runBufferDummyWindow b sizeB))

-- These methods are used to modify the contents of the NSTextStorage.
-- We do not allow direct updates of the contents this way, though.
yts_replaceCharactersInRangeWithString :: forall t. NSRange -> NSString t -> YiTextStorage () -> IO ()
yts_replaceCharactersInRangeWithString _ _ _ = return ()
yts_setAttributesRange :: forall t. NSDictionary t -> NSRange -> YiTextStorage () -> IO ()
yts_setAttributesRange _ _ _ = return ()



-- | Obtain the maximal range with a consistent style.
--   This negatively assumes that any two adjacent strokes
--   have different styles, and positively assumes that
--   the start of all strokes are the same.
minimalStyle :: UIStyle -> Point -> [[Stroke]] -> (Point,Style)
minimalStyle sty q xs =
  (\ (es, ss) -> (minimum (q:es), concat ss)) $ unzip [ (e,s sty) | (_, s, e):_ <- xs ]

-- | Obtain the maximal range for a particular attribute.
minimalAttr :: UIStyle -> (Style -> Maybe Attr) -> [[Stroke]] -> Maybe (Point, Attr)
minimalAttr sty f xs =
  listToMaybe $ catMaybes [ (,) <$> pure b <*> f (s sty) | (_, s, b):_ <- xs ]

-- | Use this with minimalAttr to get background information
background :: Style -> Maybe Attr
background s = listToMaybe [x | x@(Background _) <- s]


-- TODO: Integrate defaults into below, and cache(?)
-- | Convert style information into Cocoa compatible format
convertStyle :: Style -> IO (NSDictionary ())
convertStyle s = do
  d <- castObject <$> dictionary _NSMutableDictionary
  ft <- userFixedPitchFontOfSize 0 _NSFont
  setValueForKey ft nsFontAttributeName d
  fillStyleDict d s
  castObject <$> return d

-- | Fill and return the filled dictionary with the style information
fillStyleDict :: NSMutableDictionary t -> Style -> IO ()
fillStyleDict _ [] = return ()
fillStyleDict d (x:xs) = do
  fillStyleDict d xs
  getDictStyle x >>= flip (uncurry setValueForKey) d

-- | Return a (value, key) pair for insertion into the style dictionary
getDictStyle :: Attr -> IO (NSColor (), NSString ())
getDictStyle (Foreground c) = (,) <$> getColor True c  <*> pure nsForegroundColorAttributeName
getDictStyle (Background c) = (,) <$> getColor False c <*> pure nsBackgroundColorAttributeName

-- | Convert a Yi color into a Cocoa color
getColor :: Bool -> Color -> IO (NSColor ())
getColor fg Default = if fg then _NSColor # blackColor else _NSColor # whiteColor
getColor fg Reverse = if fg then _NSColor # whiteColor else _NSColor # blackColor
getColor _g (RGB r g b) =
  let conv = (/255) . fromIntegral in
  _NSColor # colorWithDeviceRedGreenBlueAlpha (conv r) (conv g) (conv b) 1.0

-- | A version of poke that does nothing if p is null.
safePoke :: (Storable a) => Ptr a -> a -> IO ()
safePoke p x = if p == nullPtr then return () else poke p x

-- | Execute strokeRangesB on the buffer, and update the buffer
--   so that we keep around cached syntax information...
runStrokesAround :: CUInt -> YiTextStorage () -> IO [[Stroke]]
runStrokesAround i self = do
  Just b <- self #. _buffer
  let p = fromIntegral i
  let (strokes,b') = runBuffer (dummyWindow $ bkey b) b (strokesRangesB Nothing p (p + strokeRangeExtent))
  self # setIVar _buffer (Just b')
  return strokes


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
  buf # setMonospaceFont
  return buf

setTextStorageBuffer :: FBuffer -> TextStorage -> IO ()
setTextStorageBuffer buf storage = do
  logPutStrLn $ "setTextStorageBuffer! " ++ show [u | TextUpdate u <- pendingUpdates buf]
  if null (pendingUpdates buf)
    then return ()
    else do
      storage # beginEditing
      mapM_ (applyUpdate storage) ([u | TextUpdate u <- pendingUpdates buf])
      storage # setIVar _buffer (Just buf)
      storage # setIVar _stringCache Nothing
      storage # setIVar _attributesCache Nothing
      storage # endEditing

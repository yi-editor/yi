{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -funbox-strict-fields #-} -- we might as well unbox our Ints.

-- | This module defines the layout manager interface (see 'LayoutManager'). To desgin a new layout manager, just make an instance of this class.
module Yi.Layout
  (
    -- * Concrete layouts
    Layout(..),
    Orientation(..),
    DividerPosition,
    DividerRef,
    RelativeSize,
    dividerPositionA,

    -- * Layout managers
    -- ** The interface
    LayoutManager(..),
    AnyLayoutManager(..),
    layoutManagerSameType,
    -- ** Standard managers
    wide,
    tall,
    slidyTall,
    slidyWide,
    hPairNStack,
    vPairNStack,
    -- * Utility functions
    -- ** Layouts as rectangles
    Rectangle(..),
    layoutToRectangles,
    -- ** Transposing things
    Transposable(..),
    Transposed(..),
    -- ** 'DividerRef' combinators
    -- $divRefCombinators
    LayoutM,
    pair,
    singleWindow,
    stack,
    evenStack,
    runLayoutM,
  )
 where

import           Control.Applicative        (pure, (<$>), (<*>), (<|>))
import           Control.Arrow              (first)
import           Control.Lens               (Lens', lens)
import qualified Control.Monad.State.Strict as Monad (State, evalState, get, put)
import           Data.Default               (Default, def)
import           Data.List                  (foldl', mapAccumL)
import           Data.Maybe                 (fromMaybe)
import           Data.Typeable              (Typeable, cast, typeOf)

-------------------------------- Some design notes ----------------------
-- [Treatment of mini windows]

-- Mini windows are not subject to layout; instead, they are always
-- placed at the bottom of the screen. There are multiple reasons for
-- this, as discussed in
-- https://groups.google.com/d/topic/yi-devel/vhTObC25dpY/discussion, one
-- being that for many layouts, the bottom (or top) of the screen is the
-- only reasonable place for mini windows (for example, think about
-- side-by-side layouts).

-- [Design of the 'Layout' datatype]

-- The 'Layout' datatype is currently implemented in terms of
-- horizontal stacks and vertical stacks. An alternative approach,
-- which xmonad uses, is the following: a 'Layout a' could be a
-- function @a -> Rectangle@ which specifies in coordinates where a
-- window should be placed.
--
-- While this alternative is more flexible than the current approach
-- in allowing spiral layouts and the like, the vty UI doesn't support
-- this: only vertical and horizontal composition of images is
-- allowed.



----------------------------------- Concrete 'Layout's.
-- | UI-agnostic layout schema. The basic constructs are
-- (horizontal/vertical) stacks with fixed ratios between window
-- sizes; and (horizontal/vertical) pairs with a slider in between (if
-- available).
data Layout a
  = SingleWindow a
  | Stack {
      orientation :: !Orientation,              -- ^ Orientation
      wins        :: [(Layout a, RelativeSize)] -- ^ The layout stack, with the given weights
        -- TODO: fix strictness for stack (it's still lazy)
      }
  | Pair {
       orientation :: !Orientation,     -- ^ Orientation
       divPos      :: !DividerPosition, -- ^ Initial position of the divider
       divRef      :: !DividerRef,      -- ^ Index of the divider (for updating the divider position)
       pairFst     :: !(Layout a),      -- ^ Upper of of the pair
       pairSnd     :: !(Layout a)       -- ^ Lower of the pair
    }
  deriving(Typeable, Eq, Functor)

-- | Accessor for the 'DividerPosition' with given reference
dividerPositionA :: DividerRef -> Lens' (Layout a) DividerPosition
dividerPositionA ref = lens getter (flip setter) where
  setter pos = set'
    where
      set' s@(SingleWindow _) = s
      set' p@Pair{} | divRef p == ref = p{ divPos = pos }
                    | otherwise       = p{ pairFst = set' (pairFst p), pairSnd = set' (pairSnd p) }
      set' s@Stack{} = s{ wins = fmap (first set') (wins s) }

  getter = fromMaybe invalidRef . get'

  get' (SingleWindow _) = Nothing
  get' p@Pair{} | divRef p == ref = Just (divPos p)
                | otherwise       = get' (pairFst p) <|> get' (pairSnd p)
  get' s@Stack{} = foldl' (<|>) Nothing (fmap (get' . fst) (wins s))

  invalidRef = error "Yi.Layout.dividerPositionA: invalid DividerRef"

instance Show a => Show (Layout a) where
  show (SingleWindow a) = show a
  show (Stack o s) = show o ++ " stack " ++ show s
  show p@(Pair{}) = show (orientation p) ++ " " ++ show (pairFst p, pairSnd p)

-- | The def layout consists of a single window
instance Default a => Default (Layout a) where
  def = SingleWindow def

-- | Orientations for 'Stack' and 'Pair'
data Orientation
  = Horizontal
  | Vertical
  deriving(Eq, Show)

-- | Divider reference
type DividerRef = Int

-- | Divider position, in the range (0,1)
type DividerPosition = Double

-- | Relative sizes, for 'Stack'
type RelativeSize = Double

----------------------------------------------------- Layout managers
-- TODO: add Binary requirement when possible
-- | The type of layout managers. See the layout managers 'tall', 'hPairNStack' and 'slidyTall' for some example implementations.
class (Typeable m, Eq m) => LayoutManager m where
  -- | Given the old layout and the new list of windows, construct a
  -- layout for the new list of windows.
  --
  -- If the layout manager uses sliding dividers, then a user will expect that most
  -- of these dividers don't move when adding a new window. It is the layout
  -- manager's responsibility to ensure that this is the case, and this is the
  -- purpose of the @Layout a@ argument.
  --
  -- The old layout may come from a different layout manager, in which case the layout manager is free to ignore it.
  pureLayout :: m -> Layout a -> [a] -> Layout a
  -- | Describe the layout in a form suitable for the user.
  describeLayout :: m -> String
  -- | Cycles to the next variant, if there is one (the default is 'id')
  nextVariant :: m -> m
  nextVariant = id
  -- | Cycles to the previous variant, if there is one (the default is 'id'
  previousVariant :: m -> m
  previousVariant = id

-- | Existential wrapper for 'Layout'
data AnyLayoutManager = forall m. LayoutManager m => AnyLayoutManager !m
  deriving(Typeable)

instance Eq AnyLayoutManager where
  (AnyLayoutManager l1) == (AnyLayoutManager l2) = maybe False (== l2) (cast l1)

instance LayoutManager (AnyLayoutManager) where
  pureLayout (AnyLayoutManager l) = pureLayout l
  describeLayout (AnyLayoutManager l) = describeLayout l
  nextVariant (AnyLayoutManager l) = AnyLayoutManager (nextVariant l)
  previousVariant (AnyLayoutManager l) = AnyLayoutManager (previousVariant l)

-- | The default layout is 'tallLayout'
instance Default AnyLayoutManager where
  def = hPairNStack 1

-- | True if the internal layout managers have the same type (but are not necessarily equal).
layoutManagerSameType :: AnyLayoutManager -> AnyLayoutManager -> Bool
layoutManagerSameType (AnyLayoutManager l1) (AnyLayoutManager l2) = typeOf l1 == typeOf l2

------------------------------ Standard layouts
-- | Tall windows (i.e. places windows side-by-side, equally spaced)
data Tall = Tall
  deriving(Eq, Typeable)

-- | Windows placed side-by-side, equally spaced.
tall :: AnyLayoutManager
tall = AnyLayoutManager Tall

instance LayoutManager Tall where
  pureLayout Tall _oldLayout ws = runLayoutM $ evenStack Horizontal (fmap singleWindow ws)
  describeLayout Tall = "Windows positioned side-by-side"

-- | Wide windows (windows placed on top of one another, equally spaced)
data Wide = Wide
  deriving(Eq, Typeable)

instance LayoutManager Wide where
  pureLayout Wide _oldLayout ws = runLayoutM $ evenStack Vertical (fmap singleWindow ws)
  describeLayout Wide = "Windows positioned above one another"

-- | Windows placed on top of one another, equally spaced
wide :: AnyLayoutManager
wide = AnyLayoutManager Wide

-- | Tall windows, with arranged in a balanced binary tree with sliders in between them
data SlidyTall = SlidyTall
  deriving(Eq, Typeable)

-- | Tall windows, arranged in a balanced binary tree with sliders in between them.
slidyTall :: AnyLayoutManager
slidyTall = AnyLayoutManager SlidyTall

instance LayoutManager SlidyTall where
  -- an error on input [] is easier to debug than an infinite loop.
  pureLayout SlidyTall _oldLayout [] = error "Yi.Layout: empty window list unexpected"
  pureLayout SlidyTall oldLayout xs = runLayoutM (go (Just oldLayout) xs) where
     go _layout [x] = singleWindow x
     go layout (splitList -> (lxs, rxs)) =
       case layout of
           -- if the old layout had a pair in the same point of the tree, use its divider position
           Just (Pair Horizontal pos _ l r) -> pair Horizontal pos (go (Just l) lxs) (go (Just r) rxs)
           -- otherwise, just use divider position 0.5
           _ -> pair Horizontal 0.5 (go Nothing lxs) (go Nothing rxs)

  describeLayout SlidyTall = "Slidy tall windows, with balanced-position sliders"

splitList :: [a] -> ([a], [a])
splitList xs = splitAt ((length xs + 1) `div` 2) xs

-- | Transposed version of 'SlidyTall'
newtype SlidyWide = SlidyWide (Transposed SlidyTall)
  deriving(Eq, Typeable)

-- | Transposed version of 'slidyTall'
slidyWide :: AnyLayoutManager
slidyWide = AnyLayoutManager (SlidyWide (Transposed SlidyTall))

instance LayoutManager SlidyWide where
    pureLayout (SlidyWide w) = pureLayout w
    describeLayout _ = "Slidy wide windows, with balanced-position sliders"

-- | Fixed number of \"main\" windows on the left; stack of windows on the right
data HPairNStack = HPairNStack !Int
  deriving(Eq, Typeable)

-- | @n@ windows on the left; stack of windows on the right.
hPairNStack :: Int -> AnyLayoutManager
hPairNStack n | n < 1     = error "Yi.Layout.hPairNStackLayout: n must be at least 1"
                    | otherwise = AnyLayoutManager (HPairNStack n)

instance LayoutManager HPairNStack where
    pureLayout (HPairNStack n) oldLayout (fmap singleWindow -> xs)
          | length xs <= n = runLayoutM $ evenStack Vertical xs
          | otherwise = runLayoutM $ case splitAt n xs of
              (ls, rs) ->  pair Horizontal pos
                 (evenStack Vertical ls)
                 (evenStack Vertical rs)
       where
          pos = case oldLayout of
              Pair Horizontal pos' _ _ _ -> pos'
              _ -> 0.5

    describeLayout (HPairNStack n) = show n ++ " windows on the left; remaining windows on the right"
    nextVariant (HPairNStack n) = HPairNStack (n+1)
    previousVariant (HPairNStack n) = HPairNStack (max (n-1) 1)

newtype VPairNStack = VPairNStack (Transposed HPairNStack)
  deriving(Eq, Typeable)

-- | Transposed version of 'hPairNStack'.
vPairNStack :: Int -> AnyLayoutManager
vPairNStack n = AnyLayoutManager (VPairNStack (Transposed (HPairNStack n)))

instance LayoutManager VPairNStack where
    pureLayout (VPairNStack lm) = pureLayout lm
    previousVariant (VPairNStack lm) = VPairNStack (previousVariant lm)
    nextVariant (VPairNStack lm) = VPairNStack (nextVariant lm)
    describeLayout (VPairNStack (Transposed (HPairNStack n))) = show n ++ " windows on top; remaining windows beneath"

----------------------- Utils

-- | A general bounding box
data Rectangle = Rectangle { rectX, rectY, rectWidth, rectHeight :: !Double }
  deriving(Eq, Show)

layoutToRectangles :: Rectangle -> Layout a -> [(a, Rectangle)]
layoutToRectangles bounds (SingleWindow a) = [(a, bounds)]
layoutToRectangles bounds (Stack o ts) = handleStack o bounds ts
layoutToRectangles bounds (Pair o p _ a b) = handleStack o bounds [(a,p), (b,1-p)]

handleStack :: Orientation -> Rectangle -> [(Layout a, RelativeSize)] -> [(a, Rectangle)]
handleStack o bounds tiles =
      let (totalSpace, startPos, mkBounds) = case o of
            Vertical -> (rectHeight bounds, rectY bounds, \pos size -> bounds{rectY = pos, rectHeight=size})
            Horizontal -> (rectWidth bounds, rectX bounds, \pos size -> bounds{rectX = pos, rectWidth=size})

          totalWeight' = sum (fmap snd tiles)
          totalWeight = if totalWeight' > 0 then totalWeight' else error "Yi.Layout: Stacks must have positive weights"
          spacePerWeight = totalSpace / totalWeight
          doTile pos (t, wt) = (pos + wt * spacePerWeight,
                                layoutToRectangles (mkBounds pos (wt * spacePerWeight)) t)
      in
       concat . snd . mapAccumL doTile startPos $ tiles

----------- Flipping things
-- | Things with orientations which can be flipped
class Transposable r where transpose :: r -> r
instance Transposable Orientation where { transpose Horizontal = Vertical; transpose Vertical = Horizontal }
instance Transposable (Layout a) where
    transpose (SingleWindow a) = SingleWindow a
    transpose (Stack o ws) = Stack (transpose o) (fmap (first transpose) ws)
    transpose (Pair o p r a b) = Pair (transpose o) p r (transpose a) (transpose b)

-- | Same as 'lm', but with all 'Orientation's 'transpose'd. See 'slidyWide' for an example of its use.
newtype Transposed lm = Transposed lm
  deriving(Eq, Typeable)

instance LayoutManager lm => LayoutManager (Transposed lm) where
    pureLayout (Transposed lm) l ws = transpose (pureLayout lm (transpose l) ws)
    describeLayout (Transposed lm) = "Transposed version of: " ++ describeLayout lm
    nextVariant (Transposed lm) = Transposed (nextVariant lm)
    previousVariant (Transposed lm) = Transposed (previousVariant lm)

-------------------- 'DividerRef' combinators
-- $divRefCombinators
-- It is tedious and error-prone for 'LayoutManager's to assign 'DividerRef's themselves. Better is to use these monadic smart constructors for 'Layout'. For example, the layout
--
-- @'Pair' 'Horizontal' 0.5 0 ('Pair' 'Vertical' 0.5 1 ('SingleWindow' w1) ('SingleWindow' w2)) ('SingleWindow' w3)@
--
-- could be with the combinators below as
--
-- @'runLayoutM' $ 'pair' 'Horizontal' 0.5 ('pair' 'Vertical' 0.5 ('singleWindow' w1) ('singleWindow' w2)) ('singleWindow' w3)@
--
-- These combinators do will also ensure strictness of the 'wins' field of 'Stack'. They also tidy up and do some error checking: length-1 stacks are removed (they are unnecessary); length-0 stacks raise errors.

-- | A 'Layout a' wrapped in a state monad for tracking 'DividerRef's. This type is /not/ itself a monad, but should rather be thought of as a 'DividerRef'-free version of the 'Layout' type.
newtype LayoutM a = LayoutM (Monad.State DividerRef (Layout a))

singleWindow :: a -> LayoutM a
singleWindow a = LayoutM (pure (SingleWindow a))

pair :: Orientation -> DividerPosition -> LayoutM a -> LayoutM a -> LayoutM a
pair o p (LayoutM l1) (LayoutM l2) = LayoutM $ do
    ref <- Monad.get
    Monad.put (ref+1)
    Pair o p ref <$> l1 <*> l2

stack :: Orientation -> [(LayoutM a, RelativeSize)] -> LayoutM a
stack _ [] = error "Yi.Layout: Length-0 stack"
stack _ [l] = fst l
stack o ls = LayoutM (Stack o <$> mapM (\(LayoutM lm,rs) -> (,rs) <$> lm) ls)

-- | Special case of 'stack' with all 'RelativeSize's equal.
evenStack :: Orientation -> [LayoutM a] -> LayoutM a
evenStack o ls = stack o (fmap (\l -> (l,1)) ls)

runLayoutM :: LayoutM a -> Layout a
runLayoutM (LayoutM l) = Monad.evalState l 0

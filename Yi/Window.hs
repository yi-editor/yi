--
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--

--
-- | A 'Window' is a rectangular view onto a buffer. Editor actions
-- manipulate buffers via particular windows onto that buffer. Some
-- editor actions, such as scrolling, only manipulate windows, without
-- touching the underlying buffer.
--

module Yi.Window where

import Yi.Buffer
import Yi.FastBuffer            ( FBuffer ) -- for specialisation

import Data.Char                ( isLatin1 )
import Data.Unique              ( Unique, newUnique )
import Control.Monad            ( when )

--
-- | A window onto a buffer.
--
-- Windows need some small information about the terminal. For example,
-- they need to know the height and width.  That's about it.
--
-- This doesn't handle line wrapping, yet.
--
data Window =
    Window {
        key         :: !Unique         -- ^ each window has a unique
       ,bufkey      :: !Unique         -- ^ the buffer this window opens to
       ,mode        :: !(Maybe String) -- ^ this window's modeline
       ,height      :: !Int            -- ^ height of this window
       ,width       :: !Int            -- ^ width of this window

       ,cursor      :: !(Int,Int)      -- ^ cursor point on screen

       ,pnt         :: !Int            -- ^ current point
       ,lineno      :: !Int            -- ^ current line number

       ,tospnt      :: !Int            -- ^ the buffer point of the top of screen
       ,toslineno   :: !Int            -- ^ line number of top of screen
    }


instance Eq Window where
    Window { key = u } == Window { key = v }  = u == v

instance Ord Window where
    Window { key = u } `compare` Window { key = v }  = u `compare` v
    Window { key = u } <     Window { key = v }      = u <     v

-- ---------------------------------------------------------------------
-- Construction

--
-- A new window
--
-- The origin, height and width should be calculated with reference to
-- all existing windows.
--
emptyWindow :: Buffer a => a -> (Int,Int) -> IO Window
emptyWindow b (h,w) = do
    wu <- newUnique
    let win = Window {
                    key       = wu
                   ,bufkey    = (keyB b)
                   ,mode      = Nothing
                   ,height    = h-1    -- - 1 for the cmdline?
                   ,width     = w
                   ,cursor    = (0,0)  -- (y,x) (screen columns, needs to know about tabs)
                   ,pnt       = 0      -- cache point when out of focus
                   ,lineno    = 1
                   ,tospnt    = 0
                   ,toslineno = 1      -- start on line 1
              }
    return win

------------------------------------------------------------------------
--
-- | Given a buffer, and some information update the modeline
-- There's some useful code in textinfo.window.c. Worth a read.
--
-- N.B. the contents of modelines should be specified by keymaps, and
-- not hardcoded.
--
updateModeLine :: Buffer a => Window -> a -> IO (Maybe String)
updateModeLine w' b = do
    if mode w' == Nothing then return Nothing else do
    let f    = nameB b
        ln   = lineno w'
        lns  = show ln
        top  = toslineno w'
        cols = show $ 1 + snd (cursor w')

    p <- if ln == 0 then return 0 else indexOfEol b
    s <- sizeB b
    let pct = if top == 1 then "Top" else getPercent p s

    case flip replicate ' ' (16 - length cols - length pct) of { spc' ->
    case flip replicate ' ' (width w' - (3 + sum
                (map length [f,lns,cols,pct,spc']))) of { spaces ->
    return $ Just $ "\"" ++ f ++ "\"" ++ spaces ++
                    lns ++ "," ++ cols ++ spc' ++ pct
    }}

--
-- | Give a point, and the file size, gives us a percent string
--
getPercent :: Int -> Int -> String
getPercent a b = show p ++ "%"
    where p = ceiling ((fromIntegral a) / (fromIntegral b) * 100 :: Double) :: Int

-- ---------------------------------------------------------------------
-- Window actions. These are, often, wrappers around the underlying
-- buffer actions.  Some actions, such as scrolling, are just windows
-- only. When we modify the buffer state, we must be sure to keep the
-- window up to date as well.
--

--
-- | The point moves backwards in the buffer, but the screen doesn't
-- scroll, until we reach the top of the screen.
--
moveUpW :: Buffer a => Window -> a -> IO Window
moveUpW w b | lineno w == 1 = return w
            | otherwise     = lineUp b >> decY w b >>= flip update b

--
-- | The cursor moves up, staying with its original line, unless it
-- reaches the top of the screen.
--
moveDownW :: Buffer a => Window -> a -> IO Window
moveDownW w b = do
    ll <- atLastLine b
    if ll then return w      -- eof, go no further
          else lineDown b >> incY w b >>= flip update b

-- ---------------------------------------------------------------------
-- | Wacky moveToW function

-- roughly resetPoint. so refactor!

moveToW :: Buffer a => Int -> Window -> a -> IO Window
moveToW np w b = do
    moveTo b np
    newln <- curLn b
    let gap = newln - toslineno w
        off   = if mode w == Nothing then 0 else 1
    case () of {_
        | gap < height w - off - 1 && newln >= toslineno w -- still on the screen
        -> resetW w b newln (newln - toslineno w)

        | otherwise                             -- dump on bottom line :(
        -> resetW w b newln (min (newln-1) (height w - off - 1 ))
    }

-- | goto an arbitrary line in the file. center that line on the screen
-- gotoLn is (fast as possible) an O(n) op atm.
--
gotoLnW :: Buffer a => Int -> Window -> a -> IO Window
gotoLnW n w b = do
    ln <- gotoLn b n
    resetW w b ln (min (ln-1) ((height w) `div` 2))

--
-- | Goto a line offset from the current line
--
gotoLnFromW :: Buffer a => Int -> Window -> a -> IO Window
gotoLnFromW n w b = do
    ln <- gotoLnFrom b n
    resetW w b ln 0

------------------------------------------------------------------------
--
-- | reset the window to the current line
-- @gap@ is where to center the window.
--
resetW :: Buffer a => Window -> a -> Int -> Int -> IO Window
resetW w b ln gap = do
    p  <- pointB b      -- see where it actually got placed
    x  <- offsetFromSol b
    tw <- expandedTabLengthB b 8        -- hard coded for now
    let topln = ln - gap
    i <- indexOfSolAbove b gap
    let w' = w {pnt = p, lineno = ln,
                toslineno = topln, tospnt = i,
                cursor = (gap,x-tw)}
    m <- updateModeLine w' b
    return w' { mode = m }

------------------------------------------------------------------------
--
-- | Move the cursor left or start of line
--
leftOrSolW :: Buffer a => Window -> a -> IO Window
leftOrSolW w b = moveXorSol b 1     >> update w b

--
-- | Move the cursor right or end of line
--
rightOrSolW :: Buffer a => Window -> a -> IO Window
rightOrSolW w b = moveXorEol b 1    >> update w b

-- ---------------------------------------------------------------------
-- X-axis movement
--
-- This is all stupid. We should just have a refreshW function, that
-- works out whether to reset the X or Y values.
--

-- | Move to the start of the line
moveToSolW :: Buffer a => Window -> a -> IO Window
moveToSolW w b = moveToSol b >> update w b

-- | Move to the end of the line
moveToEolW :: Buffer a => Window -> a -> IO Window
moveToEolW w b = moveToEol b >> update w b

-- | Move left @n@ or start of line
moveXorSolW :: Buffer a => Int -> Window -> a -> IO Window
moveXorSolW i w b = moveXorSol b i >> update w b

-- | Move right @n@ or end of line
moveXorEolW :: Buffer a => Int -> Window -> a -> IO Window
moveXorEolW i w b = moveXorEol b i >> update w b

-- ---------------------------------------------------------------------
-- Editing operations

--
-- | Insert a single character, shift point right to eol
-- If we insert a \n, then move to the start of the new line
--
insertW :: Buffer a => Char -> Window -> a -> IO Window
insertW c w b = do
    case c of                               -- insertB doesn't change the point
        '\13'          -> insertB b '\n'
        _ | isLatin1 c -> insertB b c
          | otherwise  -> return ()
    w' <- moveXorEolW 1 w b                       -- and shift the point
    if c == '\13' || c == '\n'      -- newline, so move down with new line
        then moveDownW w' b >>= flip moveToSolW b
        else return w'

-- | Insert a whole String at the point.
-- is this going to reset things properly?
insertNW :: Buffer a => String -> Window -> a -> IO Window
insertNW cs w b = do
    let cs' = [if c == '\13' then '\n' else c | c <- cs, isLatin1 c]
    insertN b cs'
    rightN b (length cs')
    update w b

--
-- | Delete character. Don't move point unless at EOF
--
-- In vi, you can't delete past the sol, here however, you can keep
-- deleteing till you have an empty file.
--
-- TODO think about end of file situation.
--
deleteNW :: Buffer a => Window -> a -> Int -> IO Window
deleteNW w b i = do

    -- delete up to eof chars
    when (i > 1) $ do
        rightB b
        deleteN b (i-1)
        eof <- atEof b
        when (not eof) $ leftB b

    sof <- atSof b
    eof <- atEof b  -- are we going to del eof
    sol <- atSol b  -- and we're not on the same line?

    deleteB b

    if eof && sol && not sof
        then moveToEolW w b >>= flip decY b  -- todo should handle 0
        else update w b

deleteNAtW :: Buffer a => Window -> a -> Int -> Int -> IO Window
deleteNAtW w b i p = deleteNAt b i p >> update w b

--
-- | Kill all the characters to the end of the line
-- If there is no \n at the end of the line, scroll up 1
--
deleteToEolW :: Buffer a => Window -> a -> IO Window
deleteToEolW w b = do
    sol  <- atSol b      -- about to delete from sol to eol
    sof  <- atSof b
    noNl <- noNLatEof b  -- and no \n at eol
    deleteToEol b
    if noNl && sol && not sof
        then moveToEolW w b >>= flip decY b
        else update w b

------------------------------------------------------------------------
--
-- | update window point and cursor, and reset pnt cache
-- A lot of time is spent here
--
update :: Buffer a => Window -> a -> IO Window
update w b = do
    y  <- curLn b 
    x  <- offsetFromSol b
    tw <- expandedTabLengthB b 8
    p  <- pointB b

    -- compute the new top of screen. (handle scrolling)
    let newtoslineno | y < toslineno w                = y
                     | y > toslineno w + height w - 1 = y - height w + 1
                     | otherwise                      = toslineno w
    gotoLn b newtoslineno
    newtospnt <- pointB b
    moveTo b p
    let w' = w { pnt = p, 
                  lineno = y,
                  tospnt = newtospnt,
                  toslineno = newtoslineno, 
                  cursor = (y - newtoslineno, x + tw)
                }
    m <- updateModeLine w' b
    return $! w' { mode = m }
{-# INLINE update #-}
{-# SPECIALIZE update :: Window -> FBuffer -> IO Window #-}

--
-- | Decrememt the y-related values. Might change top of screen point
-- TODO deal with sof
--
decY :: Buffer a => Window -> a -> IO Window
decY w b = do
    p <- pointB b           -- current point
    x <- offsetFromSol b    -- x offset
    tw <- expandedTabLengthB b 8
    let (y,_) = cursor w
        curln = lineno w
        topln = toslineno w
        w' = w { lineno = curln-1 }
    return $ if topln < curln
             then w' { cursor = (y-1, x + tw) }             -- just move cursor
             else w' { toslineno = topln-1, tospnt = p-x }  -- or move window

--
-- | incrememt the y-related values.
-- TODO deal with eof
--
incY :: Buffer a => Window -> a -> IO Window
incY w@(Window {height=h}) b = do
   let (y,x) = cursor w
       curln = lineno w
       topln = toslineno w
       w'    = w { lineno = curln+1 }
       off   = if mode w == Nothing then 0 else 1
   t <- indexOfNLFrom b (tospnt w)
   return $ if curln - topln < h - off - 1
            then w' { cursor = (y+1,x) }                  -- just move cursor
            else w' { toslineno = topln + 1, tospnt = t } -- scroll window

------------------------------------------------------------------------

--
-- return True if we're on the last line, and there's no \n at the end
--
noNLatEof :: Buffer a => a -> IO Bool
noNLatEof b = do
    p <- pointB b
    moveToEol b
    c  <- readB b
    moveTo b p
    return (c /= '\n')

--
-- | return index of Sol on line @n@ above current line
--
indexOfSolAbove :: Buffer a => a -> Int -> IO Int
indexOfSolAbove b n = do
    p <- pointB b
    moveToSol b
    loop n b
    q <- pointB b
    moveTo b p
    return q

    where loop 0 _  = return ()
          loop i b' = lineUp b' >> loop (i-1) b'

{-# SPECIALIZE indexOfSolAbove :: FBuffer -> Int -> IO Int #-}

--
-- | Adjust the window's size-related fields, assuming a new window
-- height, and width. Now, if we know the height of the screen, and the
-- number of lines, center the line in the screen please.
--
resize :: Buffer a => Int -> Int -> Window -> a -> IO Window
resize y x w b = do
    let topln = toslineno w
        ln    = lineno w
        w'    = w { height = y, width = x }
    if ln - topln + 1 >= y          -- old line isn't on screen now
        then do let gap   = min ln (y `div` 2)
                    topln'= ln - gap
                x'<- offsetFromSol b
                tw<- expandedTabLengthB b 8
                i <- indexOfSolAbove b gap
                return w' { toslineno = topln',
                            tospnt = i,
                            cursor = (gap,max x (x' + tw)) } -- ok ?
        else return w'

--
-- | We've just gained focus. See if anything changed, and reset the
-- cursor point appropriately. The idea is that if anything changed, we
-- position the working line in the middle of the window.
--
-- code duplicated in moveToW
--
resetPoint :: Buffer a => Window -> a -> IO Window
resetPoint w b = do
    let op  = pnt w
        oln = lineno w
        y   = fst $ cursor w
    moveTo b op         -- reset the point to what we think it should be
    ln <- curLn b
    p  <- pointB b      -- see where it actually got placed
    x  <- offsetFromSol b
    tw <- expandedTabLengthB b 8
    w' <- if op /= p || oln /= ln -- then the file shrunk or line moved
          then do let gap   = min (ln-1) ((height w) `div` 2)
                      topln = ln - gap
                  i <- indexOfSolAbove b gap
                  return w {pnt = p, lineno = ln,
                            toslineno = topln, tospnt = i,
                            cursor = (gap,x + tw)}
          else return w {pnt = p, cursor = (y,x + tw)} -- just check out x-offset is right
    m <- updateModeLine w' b
    return w' { mode = m }


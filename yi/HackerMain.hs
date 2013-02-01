import Yi
import Yi.Keymap.Emacs as Emacs
import Yi.String (mapLines)

increaseIndent :: BufferM ()
increaseIndent = do
   r <- getSelectRegionB
   r' <- unitWiseRegion Line r -- extend the region to full lines.
   modifyRegionB (mapLines (' ':)) r'

comment :: Color
comment       = white
keyword       = RGB 0x8a 0xc6 0xf1
typecolor     = RGB 0x9e 0x00 0xfe
variable      = green
interface     = RGB 0xaf 0x00 0x5f
prompt        = RGB 0xaf 0x00 0x5f
stringcolor   = RGB 0x00 0xd7 0x5f

main :: IO ()
main = do
  initDebug ".yi.dbg"
  yi $ defaultEmacsConfig  {
    defaultKm =
      Emacs.mkKeymap $ override Emacs.defKeymap $ \parent _self ->
        parent {
           eKeymap = (eKeymap parent) ||> (ctrlCh 'w' ?>>! bkillWordB)
				      ||> (metaCh 'z' ?>>! undoB)
        }
  , configUI = UIConfig {
        configFontSize = Nothing
        , configFontName = Nothing
        , configScrollWheelAmount = 4
        , configScrollStyle = Nothing
        , configCursorStyle = FatWhenFocusedAndInserting
        , configLineWrap = True
        , configLeftSideScrollBar = True
        , configAutoHideScrollBar = False
        , configAutoHideTabBar = True
        , configWindowFill = ' '
        , configVtyEscDelay = 0
        , configTheme = defaultLightTheme `override` \super _ -> super {
          importStyle = withFg keyword
          , commentStyle       = withFg comment
          , blockCommentStyle  = withFg comment
          , typeStyle          = withFg typecolor
          , keywordStyle       = withFg keyword
          , numberStyle        = withFg green
          , baseAttributes     = emptyAttributes { foreground = green,    background = black }
          , miniBaseAttributes = emptyAttributes { foreground = prompt,  background = black }
          , variableStyle      = withFg green
          , stringStyle        = withFg stringcolor
          }
        }
  , debugMode = True
  -- bind M-> to increaseIndent and mix with default Emacs keymap.
  }
  

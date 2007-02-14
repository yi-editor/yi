module Yi.Keymap where

import Yi.Event
import Data.Typeable
import Control.Exception

-- ---------------------------------------------------------------------
-- | The type of user-bindable functions
--
type Action = IO ()

type Keymap = [Event] -> [Action]

--
-- Our meta exception returns the next keymap to use
--
newtype MetaActionException = MetaActionException Keymap
    deriving Typeable

--
-- | Given a keymap function, throw an exception to interrupt the main
-- loop, which will continue processing with the supplied keymap.  Use
-- this when you want to alter the keymap lexer based on the outcome of
-- some IO action. Altering the keymap based on the input to  the keymap
-- is achieved by threading a state variable in the keymap itself.
--
metaM :: Keymap -> IO ()
metaM km = throwDyn (MetaActionException km)

------------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- | The metaM action. This is our mechanism for having Actions alter
-- the current keymap. It is similar to the Ctk lexer\'s meta action.
-- It takes a new keymap to use, throws a dynamic exception, which
-- interrupts the main loop, causing it to restart with the given
-- exception. An alternative would be to change all action types to
-- @IO (Maybe Keymap)@, and check the result of each action as it is
-- forced. Currently, my feeling is that metaM will be rare enough not
-- to bother with this solution. Also, the dynamic exception solution
-- changes only a couple of lines of code.
--

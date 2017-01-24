module WMonad.Config
    ( defaultConfig
    ) where


import WMonad.Types
import WMonad.Util

import Graphics.XHB
import Graphics.XHB.KeySym.Defs

import qualified Data.Map as M


defaultButtonActions :: M.Map ([KeyButMask], ButtonIndex) (WINDOW -> W s ())
defaultButtonActions = M.fromList [(([KeyButMaskMod1], ButtonIndex1), const (logs "ButtonIndex1"))]

defaultKeyActions :: M.Map ([KeyButMask], KEYSYM) (W s ())
defaultKeyActions = M.fromList [(([KeyButMaskMod1], xK_x), logs "xK_x")]

defaultConfig :: Config ()
defaultConfig = Config
    { _buttonActionsConfig = defaultButtonActions
    , _keyActionsConfig = defaultKeyActions
    , _state0 = ()
    }

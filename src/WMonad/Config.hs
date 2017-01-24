module WMonad.Config
    ( defaultConfig
    ) where


import WMonad.Types

import Graphics.XHB
import Graphics.XHB.KeySym.Defs

import qualified Data.Map as M


defaultButtonActions :: M.Map ([ModMask], ButtonIndex) (WINDOW -> W s ())
defaultButtonActions = M.fromList [(([ModMask1], ButtonIndex1), const (return ()))]

defaultKeyActions :: M.Map ([ModMask], KEYSYM) (W s ())
defaultKeyActions = M.fromList [(([ModMask1], xK_x), return ())]

defaultConfig :: Config ()
defaultConfig = Config
    { _buttonActionsConfig = defaultButtonActions
    , _keyActionsConfig = defaultKeyActions
    , _state0 = ()
    }

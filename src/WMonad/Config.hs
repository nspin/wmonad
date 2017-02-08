module WMonad.Config
    ( defaultConfig
    ) where


import WMonad.W
import WMonad.Util

import Graphics.XHB
import Graphics.XHB.KeySym.Defs

import qualified Data.Map as M


defaultButtonActions :: M.Map ([KeyButMask], ButtonIndex) (WINDOW -> W i t s ())
defaultButtonActions = M.fromList [(([KeyButMaskMod1], ButtonIndex1), const (logs "ButtonIndex1"))]

defaultKeyActions :: M.Map ([KeyButMask], KEYSYM) (W i t s ())
defaultKeyActions = M.fromList [(([KeyButMaskMod1], xK_x), logs "xK_x")]

defaultWorkspaceTags :: [Int]
defaultWorkspaceTags = [1..9]

defaultConfig :: Config Int t ()
defaultConfig = Config
    { _buttonActionsConfig = defaultButtonActions
    , _keyActionsConfig = defaultKeyActions
    , _workspaceTags = defaultWorkspaceTags
    , _state0 = ()
    }

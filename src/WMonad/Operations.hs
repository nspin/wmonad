module WMonad.Operations
    ( grabKeys
    , grabButtons
    ) where


import WMonad.Types
import WMonad.Util.X

import Graphics.XHB
import Graphics.XHB.Monad
import Graphics.XHB.MappingState

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S


grabKeys :: W s ()
grabKeys = do
    root <- asks _rootWindow
    ks <- asks $ M.keys . _keyActions
    notify $ MkUngrabKey anyKey root [ModMaskAny]
    let grab m kc = notify $ MkGrabKey True root m kc GrabModeAsync GrabModeAsync
    forM_ ks $ \(mask, sym) -> do
         kcs <- getsMapping $ keyCodesOf sym . keyMap
         forM_ kcs $ \kc ->
            grab mask kc


grabButtons :: W s ()
grabButtons = do
    root <- asks _rootWindow
    bs <- asks $ M.keys . _buttonActions
    let grab m ix = notify $ MkGrabButton True root [EventMaskButtonPress]
                              GrabModeAsync GrabModeAsync noneId noneId ix m
    forM_ bs $ uncurry grab

module WMonad.Operations
    ( manage
    , windows
    , isClient
    , setInitialProperties
    , tileWindow
    , grabKeys
    , grabButtons
    ) where


import WMonad.Types
import WMonad.Util
import WMonad.Util.X

import Graphics.XHB
import Graphics.XHB.Monad
import Graphics.XHB.MappingState

import Data.Maybe

import Control.Lens hiding (Empty)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S


manage :: WINDOW -> W s ()
manage w = unlessM (isClient w) $ do
    windows $ current.workspace.pane.fill %~ insertFlat w


windows :: (WindowSet -> WindowSet) -> W s ()
windows f = do
    undefined


isClient :: WINDOW -> W s Bool
isClient = gets . elemOf (windowset.allClients)


setInitialProperties :: MonadX x m => WINDOW -> m ()
setInitialProperties w = do
    selectInput w clientMask
    return ()


grabKeys :: W s ()
grabKeys = do
    root <- asks _rootWindow
    ks <- asks $ M.keys . _keyActions
    notify $ MkUngrabKey anyKey root [ModMaskAny]
    let grab m kc = notify $ MkGrabKey True root m kc GrabModeAsync GrabModeAsync
    forM_ ks $ \(kbm, sym) -> do
         kcs <- getsMapping $ keyCodesOf sym . keyMap
         forM_ kcs $ grab (mapMaybe keyButToMod kbm)


grabButtons :: W s ()
grabButtons = do
    root <- asks _rootWindow
    bs <- asks $ M.keys . _buttonActions
    let grab m ix = notify $ MkGrabButton True root [EventMaskButtonPress]
                              GrabModeAsync GrabModeAsync noneId noneId ix m
    forM_ bs $ \(kbm, ix) -> do
            grab (mapMaybe keyButToMod kbm) ix

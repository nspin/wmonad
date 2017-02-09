{-# LANGUAGE MultiWayIf #-}

module WMonad.Operations
    ( manage
    , unmanage
    , windows
    , isClient
    , setInitialProperties
    , grabKeys
    , grabButtons
    ) where


import WMonad.Stack
import WMonad.Pane
import WMonad.Display
import WMonad.W
import WMonad.Geometry
import WMonad.Util
import WMonad.Util.X

import Graphics.XHB
import Graphics.XHB.Monad
import Graphics.XHB.MappingState

import Control.Lens hiding (Empty)
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Default
import qualified Data.Map as M
import qualified Data.Set as S


manage :: Default l => Client -> W t l s ()
manage cl = undefined


unmanage :: WINDOW -> W t l s ()
unmanage = undefined


windows :: (Display t l -> Display t l) -> W t l s ()
windows f = undefined


isClient :: Client -> W t l s Bool
isClient = gets . elemOf (display.allClients)

asClient :: AWindow -> (Client -> W t l s ()) -> W t l s ()
asClient (AClient cl) go = go cl
asClient _ _ = return ()

asRealClient :: AWindow -> (Client -> W t l s ()) -> W t l s ()
asRealClient (AClient cl) go = whenM (isClient cl) (go cl)
asRealClient _ _ = return ()


windowType :: WINDOW -> W t l s (Maybe AWindow)
windowType w = gets $ f . _display
  where
    f ws  | elemOf (workspaces.pane.frames ) (Frame  w) ws = Just (AFrame  (Frame  w))
          | elemOf (workspaces.pane.blanks ) (Blank  w) ws = Just (ABlank  (Blank  w))
          | elemOf (workspaces.pane.clients) (Client w) ws = Just (AClient (Client w))
          | otherwise = Nothing


hide :: AWindow -> W t l s ()
hide aw = undefined


reveal :: AWindow -> W t l s ()
reveal aw = undefined


setInitialProperties :: MonadX x m => Client -> m ()
setInitialProperties (Client w) = do
    selectInput w clientMask
    setWindowBorderWidth w 1


grabKeys :: W t l s ()
grabKeys = do
    root <- asks _rootWindow
    ks <- asks $ M.keys . _keyActions
    notify $ MkUngrabKey anyKey root [ModMaskAny]
    let grab m kc = notify $ MkGrabKey True root m kc GrabModeAsync GrabModeAsync
    forM_ ks $ \(kbm, sym) -> do
         kcs <- getsMapping $ keyCodesOf sym . keyMap
         forM_ kcs $ grab (mapMaybe keyButToMod kbm)


grabButtons :: W t l s ()
grabButtons = do
    root <- asks _rootWindow
    bs <- asks $ M.keys . _buttonActions
    let grab m ix = notify $ MkGrabButton True root [EventMaskButtonPress]
                              GrabModeAsync GrabModeAsync noneId noneId ix m
    forM_ bs $ \(kbm, ix) -> do
            grab (mapMaybe keyButToMod kbm) ix

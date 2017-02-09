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
import WMonad.Windows
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


-- TODO
manage :: Default l => WINDOW -> W t l s ()
manage w = unlessM (isClient w) $ do
    -- windows $ current.workspace.pane.fill %~ insertFlat (Right (Client w))
    undefined


unmanage :: WINDOW -> W t l s ()
unmanage = windows . over (workspaces.pane) . undefined . (/=)


windows :: (Windows i t -> Windows i t) -> W t l s ()
windows f = do
    undefined

    -- ws0 <- gets _windowset

    -- let oldVisible = ws0 ^.. visibleWorkspaces.pane.visibleLeaves
    --     newWindows = toListOf allClients ws \\ toListOf allClients ws0
    --     ws = f ws0

    -- mapM_ setInitialProperties newWindows

    -- windowset .= ws

    -- let (vt, ht, va, ha) = layoutSetSplit ws
    --     vis = map snd va

    -- mapM_ (uncurry tileWindow) va
    -- mapM_ reveal vis
    -- mapM_ hide (nub (ws0^..visibleWorkspaces.pane.visibleLeaves.raw ++ ws0^..workspaces.pane.traverse.raw) \\ vis)


isClient :: WINDOW -> W t l s Bool
isClient = (gets . elemOf (windowset.allClients)) . Client


windowType :: WINDOW -> W t l s (Maybe AWindow)
windowType w = gets $ f . _windowset
  where
    f ws  | elemOf (workspaces.pane.frames ) (Frame  w) ws = Just (AFrame  (Frame  w))
          | elemOf (workspaces.pane.blanks ) (Blank  w) ws = Just (ABlank  (Blank  w))
          | elemOf (workspaces.pane.clients) (Client w) ws = Just (AClient (Client w))
          | otherwise = Nothing


hide :: WINDOW -> W t l s ()
hide w = whenM (gets (elemOf (mappedWindows.folded) w)) $ do
    selectInput w (delete EventMaskSubstructureNotify clientMask)
    notify $ MkUnmapWindow w
    selectInput w clientMask
    waitingUnmap %= M.insertWith (+) w 1
    mappedWindows %= S.delete w


reveal :: WINDOW -> W t l s ()
reveal w = do
    notify $ MkMapWindow w
    whenM (isClient w) $ mappedWindows %= S.insert w


setInitialProperties :: MonadX x m => WINDOW -> m ()
setInitialProperties w = do
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

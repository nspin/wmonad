{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module WMonad.Core
    ( manageWindows
    ) where


import WMonad.Operations
import WMonad.Types
import WMonad.Util

import Graphics.XHB
import Graphics.XHB.Monad
import Graphics.XHB.AtomCache
import Graphics.XHB.MappingState

import Data.Array
import Data.Foldable
import Data.Maybe
import qualified Data.Map as M

import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State


manageWindows :: W s a
manageWindows = do
    grabKeys
    grabButtons
    forever $ do
        ev <- waitEvent
        let f env = env { _mousePosition = mousePositionFrom ev
                        , _currentEvent = Just ev
                        }
        local f $ fromMaybe (return ()) (handle handlers ev)


mousePositionFrom :: SomeEvent -> Maybe (Position, Position)
mousePositionFrom = handle
    [ EventHandler $ \MkKeyPressEvent{..} -> (root_x_KeyPressEvent, root_y_KeyPressEvent)
    , EventHandler $ \MkKeyReleaseEvent{..} -> (root_x_KeyReleaseEvent, root_y_KeyReleaseEvent)
    , EventHandler $ \MkEnterNotifyEvent{..} -> (root_x_EnterNotifyEvent, root_y_EnterNotifyEvent)
    , EventHandler $ \MkLeaveNotifyEvent{..} -> (root_x_LeaveNotifyEvent, root_y_LeaveNotifyEvent)
    , EventHandler $ \MkButtonPressEvent{..} -> (root_x_ButtonPressEvent, root_y_ButtonPressEvent)
    , EventHandler $ \MkButtonReleaseEvent{..} -> (root_x_ButtonReleaseEvent, root_y_ButtonReleaseEvent)
    ]


-- Event handling

data EventHandler a = forall e. Event e => EventHandler (e -> a)

handle :: [EventHandler a] -> SomeEvent -> Maybe a
handle hs ev = asum [ h `fmap` fromEvent ev | EventHandler h <- hs ]

handlers :: [EventHandler (W s ())]
handlers = [ EventHandler onKeyPress
           , EventHandler onMappingNotify
           , EventHandler onMapRequest
           , EventHandler onUnmapNotify
           ]


-- Event handlers

onKeyPress :: KeyPressEvent -> W s ()
onKeyPress e@MkKeyPressEvent{..} = do
    syms <- getsMapping ((! detail_KeyPressEvent) . keyMap)
    logp (state_KeyPressEvent, syms)
    ka <- asks _keyActions
    forM_ syms $ \sym -> do
        fromMaybe (return ()) $ M.lookup (state_KeyPressEvent, sym) ka


onMappingNotify :: MappingNotifyEvent -> W s ()
onMappingNotify e@MkMappingNotifyEvent{..} = do
    updateMapping e
    grabKeys


onMapRequest :: MapRequestEvent -> W s ()
onMapRequest MkMapRequestEvent{window_MapRequestEvent = w} = do
    logs "MapRequestEvent"
    or <- override_redirect_GetWindowAttributesReply <$> req (MkGetWindowAttributes w)
    managed <- isClient w
    unless (managed || or) $ manage w


onUnmapNotify :: UnmapNotifyEvent -> W s ()
onUnmapNotify MkUnmapNotifyEvent{window_UnmapNotifyEvent = w, from_configure_UnmapNotifyEvent = synthetic} =
    whenM (isClient w) $ do
        e <- gets (fromMaybe 0 . M.lookup w . _waitingUnmap)
        if (synthetic || e == 0)
            then unmanage w
            else waitingUnmap %= M.update mpred w
      where
        mpred 1 = Nothing
        mpred n = Just $ pred n

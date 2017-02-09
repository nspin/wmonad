{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module WMonad.Core
    ( manageWindows
    ) where


import WMonad.Operations
import WMonad.W
import WMonad.Util

import Graphics.XHB
import Graphics.XHB.Monad
import Graphics.XHB.AtomCache
import Graphics.XHB.MappingState

import Data.Array
import Data.Default
import Data.Foldable
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State


manageWindows :: Default l => W t l s a
manageWindows = do
    grabKeys
    grabButtons
    logs "OK"
    forever $ do
        ev <- waitEvent
        req MkListExtensions -- https://github.com/aslatter/xhb/issues/12
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

handlers :: Default l => [EventHandler (W t l s ())]
handlers = [ EventHandler onKeyPress
           , EventHandler onMappingNotify
           , EventHandler onMapRequest
           , EventHandler onUnmapNotify
           , EventHandler onDestroyNotify
           ]


-- Event handlers

onKeyPress :: KeyPressEvent -> W t l s ()
onKeyPress e@MkKeyPressEvent{..} = do
    syms <- getsMapping ((! detail_KeyPressEvent) . keyMap)
    ka <- asks _keyActions
    forM_ syms $ \sym -> do
        fromMaybe (return ()) $ M.lookup (state_KeyPressEvent, sym) ka


onMappingNotify :: MappingNotifyEvent -> W t l s ()
onMappingNotify e@MkMappingNotifyEvent{..} = do
    updateMapping e
    grabKeys


onMapRequest :: Default l => MapRequestEvent -> W t l s ()
onMapRequest MkMapRequestEvent{window_MapRequestEvent = w} = do
    or <- override_redirect_GetWindowAttributesReply <$> req (MkGetWindowAttributes w)
    managed <- isClient w
    unless (managed || or) $ manage w


onUnmapNotify :: UnmapNotifyEvent -> W t l s ()
onUnmapNotify MkUnmapNotifyEvent{window_UnmapNotifyEvent = w, from_configure_UnmapNotifyEvent = synthetic} =
    whenM (isClient w) $ do
        e <- gets (fromMaybe 0 . M.lookup w . _waitingUnmap)
        if (synthetic || e == 0)
            then unmanage w
            else waitingUnmap %= M.update mpred w
      where
        mpred 1 = Nothing
        mpred n = Just $ pred n


onDestroyNotify :: DestroyNotifyEvent -> W t l s ()
onDestroyNotify MkDestroyNotifyEvent{window_DestroyNotifyEvent = w} = whenM (isClient w) $ do
    unmanage w
    mappedWindows %= S.delete w
    waitingUnmap %= M.delete w

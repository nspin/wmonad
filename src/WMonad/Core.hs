{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module WMonad.Core
    ( manageWindows
    ) where


import WMonad.Types
import WMonad.Operations

import Graphics.XHB
import Graphics.XHB.Monad
import Graphics.XHB.AtomCache
import Graphics.XHB.MappingState

import Data.Foldable
import Data.Maybe

import Control.Lens

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State


manageWindows :: W s a
manageWindows = do
    undefined -- actions that happen both before and during event loop (grabbing keys, etc)
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
           , EventHandler onMapRequest
           -- ...
           ]


-- Event handlers

onKeyPress :: (MonadX IO m, MonadLogger m) => KeyPressEvent -> m ()
onKeyPress MkKeyPressEvent{..} = do
    undefined


onMapRequest :: MapRequestEvent -> W s ()
onMapRequest MkMapRequestEvent{window_MapRequestEvent = w} = do
    undefined

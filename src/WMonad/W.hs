{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module WMonad.W
    (
    -- * W
      W(..)
    , WEnv(..)
    , WState(..)
    , Position
    , Config(..)
    
    -- * Lenses
    , HasWEnv(..)
    , HasWState(..)
    , HasConfig(..)
    ) where


import WMonad.Windows

import Graphics.XHB (KEYSYM, ButtonIndex, KeyButMask, WINDOW, SomeEvent, RECTANGLE)
import Graphics.XHB.Monad
import Graphics.XHB.MappingState
import Graphics.XHB.EventQueue
import Graphics.XHB.AtomCache

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Logger

import Data.Default
import Data.Int
import Data.Traversable
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Lens


newtype W i t s a = W { unW :: LoggingT (ReaderT (WEnv i t s) (StateT (WState i t s) (MappingT (AtomCacheT (X IO))))) a }
    deriving ( Functor, Applicative, Monad
             , MonadIO, MonadReader (WEnv i t s), MonadState (WState i t s)
             , MonadLogger, MappingCtx, AtomCacheCtx
             , MonadX IO
             )


data WState i t s = WState
    { _windowset :: Windows i t
    , _mappedWindows :: S.Set WINDOW
    , _waitingUnmap :: M.Map WINDOW Int
    , _dragging :: Maybe (Position -> Position -> W i t s (), W i t s ())
    , _extra :: s
    }

data WEnv i t s = WEnv
    { _rootWindow :: WINDOW
    , _keyActions :: M.Map ([KeyButMask], KEYSYM) (W i t s ())
    , _buttonActions :: M.Map ([KeyButMask], ButtonIndex) (WINDOW -> W i t s ())
    , _mouseFocused :: Bool
    , _mousePosition :: Maybe (Position, Position)
    , _currentEvent :: Maybe SomeEvent
    }

data Config i t s = Config
    { _buttonActionsConfig :: M.Map ([KeyButMask], ButtonIndex) (WINDOW -> W i t s ())
    , _keyActionsConfig :: M.Map ([KeyButMask], KEYSYM) (W i t s ())
    , _workspaceTags :: [i]
    , _state0 :: s
    }


type Position = Int16


-- Extra Instances

instance MonadX x m => MonadX x (AtomCacheT m) where
    liftX = lift . liftX
    askX = lift askX
    throwErrorX = lift . throwErrorX
    catchErrorX (AtomCacheT m) err = undefined -- TODO

instance MonadX x m => MonadX x (LoggingT m) where
    liftX = lift . liftX
    askX = lift askX
    throwErrorX = lift . throwErrorX
    catchErrorX (LoggingT m) err = undefined -- TODO


-- Lenses

$(makeClassy ''WState)
$(makeClassy ''WEnv)
$(makeClassy ''Config)

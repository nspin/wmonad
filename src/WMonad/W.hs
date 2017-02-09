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
    , Config(..)
    , Position
    , AWindow(..)
    
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


newtype W t l s a = W { unW :: LoggingT (ReaderT (WEnv t l s) (StateT (WState t l s) (MappingT (AtomCacheT (X IO))))) a }
    deriving ( Functor, Applicative, Monad
             , MonadIO, MonadReader (WEnv t l s), MonadState (WState t l s)
             , MonadLogger, MappingCtx, AtomCacheCtx
             , MonadX IO
             )


data WState t l s = WState
    { _windowset :: Windows t l
    , _mappedWindows :: S.Set WINDOW
    , _waitingUnmap :: M.Map WINDOW Int
    , _dragging :: Maybe (Position -> Position -> W t l s (), W t l s ())
    , _extra :: s
    }

data WEnv t l s = WEnv
    { _rootWindow :: WINDOW
    , _keyActions :: M.Map ([KeyButMask], KEYSYM) (W t l s ())
    , _buttonActions :: M.Map ([KeyButMask], ButtonIndex) (AWindow -> W t l s ())
    , _mouseFocused :: Bool
    , _mousePosition :: Maybe (Position, Position)
    , _currentEvent :: Maybe SomeEvent
    }

data Config t l s = Config
    { _buttonActionsConfig :: M.Map ([KeyButMask], ButtonIndex) (AWindow -> W t l s ())
    , _keyActionsConfig :: M.Map ([KeyButMask], KEYSYM) (W t l s ())
    , _workspaceTags :: [t]
    , _state0 :: s
    }


type Position = Int16

data AWindow = AFrame Frame | ABlank Blank | AClient Client
    deriving (Eq, Show)


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

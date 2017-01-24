{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module WMonad.Types.Concrete
    (
    -- * WindowSet
      WindowSet
    , WorkspaceId
    , Portion
    , Tag
    , ScreenId
    -- * W
    , W(..)
    , WEnv(..)
    , WState(..)
    , Position
    , Config(..)
    ) where


import WMonad.Types.Abstract

import Graphics.XHB (KEYSYM, ButtonIndex, ModMask, WINDOW, SomeEvent, RECTANGLE)
import Graphics.XHB.Gen.Xinerama (ScreenInfo)
import Graphics.XHB.Monad
import Graphics.XHB.MappingState
import Graphics.XHB.EventQueue
import Graphics.XHB.AtomCache

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Logger

import Data.Int
import qualified Data.Map as M
import qualified Data.Set as S


type WindowSet = TreeSet WorkspaceId Portion Tag WINDOW ScreenId ScreenInfo

type Portion = Int
type Tag = Int

newtype WorkspaceId = WorkspaceId { getWorkspaceId :: Int }
    deriving (Eq, Ord, Show, Read, Enum, Num, Integral, Real)

newtype ScreenId = ScreenId { getScreenId :: Int }
    deriving (Eq, Ord, Show, Read, Enum, Num, Integral, Real)


newtype W s a = W { unW :: LoggingT (ReaderT (WEnv s) (StateT (WState s) (MappingT (AtomCacheT (X IO))))) a }
    deriving ( Functor, Applicative, Monad
             , MonadIO, MonadReader (WEnv s), MonadState (WState s)
             , MonadLogger, MappingCtx, AtomCacheCtx
             , MonadX IO
             )


data WState s = WState
    { _windowset :: WindowSet
    , _mapped :: S.Set WINDOW
    , _waitingUnmap :: M.Map WINDOW Int
    , _dragging :: Maybe (Position -> Position -> W s (), W s ())
    , _extra :: s
    }

data WEnv s = WEnv
    { _rootWindow :: WINDOW
    , _keyActions :: M.Map ([ModMask], KEYSYM) (W s ())
    , _buttonActions :: M.Map ([ModMask], ButtonIndex) (WINDOW -> W s ())
    , _mouseFocused :: Bool
    , _mousePosition :: Maybe (Position, Position)
    , _currentEvent :: Maybe SomeEvent
    }

data Config s = Config
    { _buttonActionsConfig :: M.Map ([ModMask], ButtonIndex) (WINDOW -> W s ())
    , _keyActionsConfig :: M.Map ([ModMask], KEYSYM) (W s ())
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

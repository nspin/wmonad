{-# LANGUAGE RecordWildCards #-}

module WMonad.Main
    ( wmonad
    ) where


import WMonad.Core
import WMonad.Types
import WMonad.Util.X

import Graphics.XHB
import Graphics.XHB.Monad
import Graphics.XHB.AtomCache
import Graphics.XHB.MappingState

import System.Exit
import System.IO
import System.Posix.Process (executeFile, forkProcess, getAnyProcessStatus, createSession)
import System.Posix.Signals

import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Exception
import Control.Lens hiding (Empty)

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State


wmonad :: Config s -> IO ()
wmonad config = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    installSignalHandlers
    mconn <- connect
    case mconn of
        Nothing -> die "failed to connect to x server"
        Just conn -> do
            end <- unX (start config) conn
            case end of
                Left err -> die (show err)
                Right _ -> return ()


start :: Config s -> X IO a
start Config{..} = do

    root <- asksX getRoot
    xinesc <- getCleanedScreenInfo

    selectInput root rootMask

    let screens = zipWith Screen [1..] xinesc
        (seen, hidden) = L.splitAt (length screens) [ Workspace i (Pane Nothing Empty) | i <- [1..9] ]
        current:visible = zipWith ($) screens seen

        ws0 = TreeSet
            { _floating = M.empty
            , _current = current
            , _visible = visible
            , _hidden = hidden
            }

        env = WEnv
            { _rootWindow = root
            , _keyActions = _keyActionsConfig
            , _buttonActions = _buttonActionsConfig
            , _mouseFocused = False
            , _mousePosition = Nothing
            , _currentEvent = Nothing
            }

        st = WState
            { _extra = _state0
            , _mapped = S.empty
            , _waitingUnmap = M.empty
            , _dragging = Nothing
            , _windowset = ws0
            }

    manageWindows & runAtomCacheT
                  . runMappingT
                  . flip evalStateT st
                  . flip runReaderT env
                  . runStderrLoggingT
                  . unW


-- Signals

installSignalHandlers :: IO ()
installSignalHandlers = void $ do
    installHandler openEndedPipe Ignore Nothing
    installHandler sigCHLD Ignore Nothing
    try go :: IO (Either SomeException ())
  where
    go = do
        x <- getAnyProcessStatus False False
        when (isJust x) go

uninstallSignalHandlers :: IO ()
uninstallSignalHandlers = void $ do
    installHandler openEndedPipe Default Nothing
    installHandler sigCHLD Default Nothing

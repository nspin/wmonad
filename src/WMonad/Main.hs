module WMonad.Main
    ( wmonad
    ) where


import WMonad.Types
import WMonad.Core

import Graphics.XHB
import Graphics.XHB.Monad
import Graphics.XHB.AtomCache
import Graphics.XHB.MappingState

import System.Exit
import System.IO
import System.Posix.Process (executeFile, forkProcess, getAnyProcessStatus, createSession)
import System.Posix.Signals

import Data.Maybe

import Control.Exception
import Control.Lens

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
start conf = do
    undefined -- collect env-related x server state, etc.
    let env = undefined
        st = undefined
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

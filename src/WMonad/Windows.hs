{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module WMonad.Windows
    ( Windows(..)
    , current
    , visible
    , hidden
    , floating

    , Screen(..)
    , screen
    , screenDetail
    , workspace

    , Workspace(..)
    , tag
    , pane

    , Leaf(..)
    , _Client
    , _Blank
    , raw

    , Portion(..)
    , ScreenId(..)

    -- * Operations
    , workspaces
    , visibleScreens
    , visibleWorkspaces
    , workspacesContaining
    , findTag
    , allClients

    ) where


import WMonad.Stack
import WMonad.Pane

import Graphics.XHB

import Control.Lens
import Data.Default
import Data.Traversable
import qualified Data.Map as M
import qualified Data.Set as S


data Windows i t = Windows
    { _current :: Screen i t
    , _visible :: [Screen i t]
    , _hidden :: [Workspace i t]
    , _floating :: M.Map WINDOW RECTANGLE
    } deriving (Eq, Show)


data Screen i t = Screen
    { _screen :: ScreenId
    , _screenDetail :: RECTANGLE
    , _workspace :: Workspace i t
    } deriving (Eq, Show)


data Workspace i t = Workspace
    { _tag :: i
    , _pane :: Pane Portion t Leaf
    } deriving (Eq, Show)


data Leaf = Client WINDOW | Blank WINDOW
    deriving (Eq, Show)

newtype Portion = Portion { getPortion :: Rational }
    deriving (Eq, Ord, Show, Read, Enum, Num, Real, Fractional, RealFrac)

instance Default Portion where
    def = 1

newtype ScreenId = ScreenId { getScreenId :: Int }
    deriving (Eq, Ord, Show, Read, Enum, Num, Integral, Real)


$(makeLenses ''Windows)
$(makeLenses ''Screen)
$(makeLenses ''Workspace)
$(makePrisms ''Leaf)


raw :: Lens' Leaf WINDOW
raw f (Client w) = Client <$> f w
raw f (Blank w) = Blank <$> f w


workspaces :: Traversal' (Windows i t) (Workspace i t)
workspaces f Windows{..} = Windows
    <$> traverseOf workspace f _current
    <*> traverseOf (traverse.workspace) f _visible
    <*> traverseOf traverse f _hidden
    <*> pure _floating


visibleScreens :: Traversal' (Windows i t) (Screen i t)
visibleScreens f Windows{..} = Windows
    <$> f _current
    <*> traverse f _visible
    <*> pure _hidden
    <*> pure _floating


visibleWorkspaces :: Traversal' (Windows i t) (Workspace i t)
visibleWorkspaces = visibleScreens . workspace


workspacesContaining :: WINDOW -> Traversal' (Windows i t) (Workspace i t)
workspacesContaining a f (Windows c v h fl) = Windows
    <$> (workspace.one) f c
    <*> (traverse.workspace.one) f v
    <*> (traverse.one) f h
    <*> pure fl
  where
    one g ws = if elemOf (pane.traverse.raw) a ws then g ws else pure ws


findTag :: WINDOW -> Traversal' (Windows i t) i
findTag a = workspacesContaining a . tag


allClients :: Fold (Windows i t) WINDOW
allClients f Windows{..} = Windows
    <$> (workspace.pane.traverse._Client) f _current
    <*> (traverse.workspace.pane.traverse._Client) f _visible
    <*> (traverse.pane.traverse._Client) f _hidden
    <*> pure _floating

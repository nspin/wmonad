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

    , ScreenId(..)

    , Workspace(..)
    , tag
    , pane

    , Frame(..)
    , Blank(..)
    , Client(..)

    -- * Operations
    , workspaces
    , visibleScreens
    , visibleWorkspaces
    , tiledClients
    , allClients

    -- , workspacesContaining
    -- , findTag

    ) where


import WMonad.Stack
import WMonad.Pane

import Graphics.XHB

import Control.Lens
import Data.Default
import Data.Traversable
import qualified Data.Map as M
import qualified Data.Set as S


data Windows t l = Windows
    { _current :: Screen t l
    , _visible :: [Screen t l]
    , _hidden :: [Workspace t l]
    , _floating :: M.Map Client RECTANGLE
    } deriving (Eq, Show)


data Screen t l = Screen
    { _screen :: ScreenId
    , _screenDetail :: RECTANGLE
    , _workspace :: Workspace t l
    } deriving (Eq, Show)


newtype ScreenId = ScreenId { getScreenId :: Int }
    deriving (Eq, Ord, Show, Read, Enum, Num, Integral, Real)


data Workspace t l = Workspace
    { _tag :: t
    , _pane :: Pane l Frame Blank Client
    } deriving (Eq, Show)


newtype Frame = Frame { unFrame :: WINDOW }
    deriving (Eq, Show)

newtype Blank = Blank { unBlank :: WINDOW }
    deriving (Eq, Show)

newtype Client = Client { unClient :: WINDOW }
    deriving (Eq, Show)


$(makeLenses ''Windows)
$(makeLenses ''Screen)
$(makeLenses ''Workspace)


workspaces :: Traversal (Windows t l) (Windows t' l') (Workspace t l) (Workspace t' l')
workspaces f Windows{..} = Windows
    <$> workspace f _current
    <*> (traverse.workspace) f _visible
    <*> traverse f _hidden
    <*> pure _floating


visibleScreens :: Traversal' (Windows i t) (Screen i t)
visibleScreens f Windows{..} = Windows
    <$> f _current
    <*> traverse f _visible
    <*> pure _hidden
    <*> pure _floating


visibleWorkspaces :: Traversal' (Windows i t) (Workspace i t)
visibleWorkspaces = visibleScreens . workspace


tiledClients :: Traversal' (Windows t l) Client
tiledClients f Windows{..} = Windows
    <$> (workspace.pane.clients) f _current
    <*> (traverse.workspace.pane.clients) f _visible
    <*> (traverse.pane.clients) f _hidden
    <*> pure _floating


allClients :: Fold (Windows t l) Client
allClients f Windows{..} = Windows
    <$> (workspace.pane.clients) f _current
    <*> (traverse.workspace.pane.clients) f _visible
    <*> (traverse.pane.clients) f _hidden
    <*> (ifolded.asIndex) f _floating


-- workspacesContaining :: WINDOW -> Traversal' (Windows t l) (Workspace t l)
-- workspacesContaining a f (Windows c v h fl) = Windows
--     <$> (workspace.one) f c
--     <*> (traverse.workspace.one) f v
--     <*> (traverse.one) f h
--     <*> pure fl
--   where
--     one g ws = if elemOf (pane.traverse.raw) a ws then g ws else pure ws


-- findTag :: WINDOW -> Traversal' (Windows t l) t
-- findTag a = workspacesContaining a . tag

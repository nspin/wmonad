{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module WMonad.Display
    ( Display(..)
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
    , AWindow(..)
    , raw
    , aframe
    , ablank
    , aclient
    
    -- * Operations

    , workspaces
    , visibleScreens
    , visibleWorkspaces
    , tiledClients
    , allClients

    , windowNodes
    , allWindows

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


data Display t l = Display
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

data AWindow = AFrame Frame | ABlank Blank | AClient Client
    deriving (Eq, Show)

raw :: AWindow -> WINDOW
raw (AFrame  (Frame  w)) = w
raw (ABlank  (Blank  w)) = w
raw (AClient (Client w)) = w

aframe :: WINDOW -> AWindow
aframe = AFrame . Frame

ablank :: WINDOW -> AWindow
ablank = ABlank . Blank

aclient :: WINDOW -> AWindow
aclient = AClient . Client


$(makeLenses ''Display)
$(makeLenses ''Screen)
$(makeLenses ''Workspace)


workspaces :: Traversal (Display t l) (Display t' l') (Workspace t l) (Workspace t' l')
workspaces f Display{..} = Display
    <$> workspace f _current
    <*> (traverse.workspace) f _visible
    <*> traverse f _hidden
    <*> pure _floating


visibleScreens :: Traversal' (Display i t) (Screen i t)
visibleScreens f Display{..} = Display
    <$> f _current
    <*> traverse f _visible
    <*> pure _hidden
    <*> pure _floating


visibleWorkspaces :: Traversal' (Display i t) (Workspace i t)
visibleWorkspaces = visibleScreens . workspace


tiledClients :: Traversal' (Display t l) Client
tiledClients f Display{..} = Display
    <$> (workspace.pane.clients) f _current
    <*> (traverse.workspace.pane.clients) f _visible
    <*> (traverse.pane.clients) f _hidden
    <*> pure _floating


floatingClients :: Fold (Display t l) Client
floatingClients f (Display c v h fl) = Display c v h <$> (ifolded.asIndex) f fl


allClients :: Fold (Display t l) Client
allClients f Display{..} = Display
    <$> (workspace.pane.clients) f _current
    <*> (traverse.workspace.pane.clients) f _visible
    <*> (traverse.pane.clients) f _hidden
    <*> (ifolded.asIndex) f _floating


windowNodes :: Fold (Pane l Frame Blank Client) AWindow
windowNodes = nodes AFrame ABlank AClient


allWindows :: Fold (Display t l) AWindow
allWindows f Display{..} = Display
    <$> (workspace.pane.windowNodes) f _current
    <*> (traverse.workspace.pane.windowNodes) f _visible
    <*> (traverse.pane.windowNodes) f _hidden
    <*> (ifolded.asIndex.go) f _floating
  where
    go f cl = cl <$ f (AClient cl) -- this has to exists somewhere in lens...


-- workspacesContaining :: WINDOW -> Traversal' (Display t l) (Workspace t l)
-- workspacesContaining a f (Display c v h fl) = Display
--     <$> (workspace.one) f c
--     <*> (traverse.workspace.one) f v
--     <*> (traverse.one) f h
--     <*> pure fl
--   where
--     one g ws = if elemOf (pane.traverse.raw) a ws then g ws else pure ws


-- findTag :: WINDOW -> Traversal' (Display t l) t
-- findTag a = workspacesContaining a . tag

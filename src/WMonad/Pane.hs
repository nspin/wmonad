{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module WMonad.Pane
    ( Pane(..)
    , label
    , frame
    , fill

    , Fill(..)
    , _Leaf
    , _Branch

    , Layout(..)

    , Part(..)
    , portion
    , content

    , Portion(..)

    -- * Operations

    , frabels
    , labels
    , frames

    , leaves
    , blanks
    , clients

    , nodes

    , visibleFrabels
    , visibleLabels
    , visibleFrames

    , visibleLeaves
    , visibleBlanks
    , visibleClients

    , justVisible

    , insertFlat

    ) where


import WMonad.Stack

import Control.Lens hiding ((<.>))
import Data.Default
import Data.Functor
import Data.Functor.Apply
import Data.Traversable


data Pane l f b c = Pane
    { _label :: l
    , _frame :: f
    , _fill :: Fill l f b c
    } deriving (Eq, Show, Read)


data Fill l f b c = Leaf (Either b c) | Branch Layout (Stack (Part l f b c))
    deriving (Eq, Show, Read)


data Layout = Stacked | Horizontal | Vertical
    deriving (Eq, Show, Read)


data Part l f b c = Part
    { _portion :: Portion
    , _content :: Pane l f b c
    } deriving (Eq, Show, Read)


newtype Portion = Portion { getPortion :: Rational }
    deriving (Eq, Ord, Show, Read, Enum, Num, Real, Fractional, RealFrac)

instance Default Portion where
    def = 1


$(makeLenses ''Pane)
$(makeLenses ''Part)
$(makePrisms ''Fill)


frabels :: Traversal (Pane l f b c) (Pane l' f' b c) (l, f) (l', f')
frabels f (Pane label frame fill) = uncurry Pane <$> f (label, frame) <*> case fill of
    Leaf e -> pure (Leaf e)
    Branch layout stack -> Branch layout <$> (traverse.content.frabels) f stack


labels :: Traversal (Pane l f b c) (Pane l' f b c) l l'
labels = frabels._1

frames :: Traversal (Pane l f b c) (Pane l f' b c) f f'
frames = frabels._2

leaves :: Traversal (Pane l f b c) (Pane l f b' c') (Either b c) (Either b' c')
leaves f (Pane label frame fill) = Pane label frame <$> case fill of
    Leaf e -> Leaf <$> f e
    Branch layout stack -> Branch layout <$> (traverse.content.leaves) f stack


blanks :: Traversal (Pane l f b c) (Pane l f b' c) b b'
blanks = leaves._Left

clients :: Traversal (Pane l f b c) (Pane l f b c') c c'
clients = leaves._Right


nodes :: (f -> a) -> (b -> a) -> (c -> a) -> Fold (Pane l f b c) a
nodes x y z = go
  where
    go f (Pane label frame fill) = Pane label <$> (frame <$ f (x frame)) <*> case fill of
        Leaf (Left  b) -> Leaf . Left  <$> (b <$ f (y b))
        Leaf (Right c) -> Leaf . Right <$> (c <$ f (z c))
        Branch layout stack -> Branch layout <$> (traverse.content.go) f stack


instance Eq l => Ixed (Pane l f b c) where
    ix l f (Pane l' frame fill) = uncurry (Pane l') <$>
        if l == l'
        then f (frame, fill)
        else case fill of
            Branch layout stack -> (,) frame . Branch layout <$> (traverse.content.ix l) f stack
            leaf -> pure (frame, leaf)

type instance Index (Pane l f b c) = l
type instance IxValue (Pane l f b c) = (f, Fill l f b c)


visibleFrabels :: Traversal' (Pane l f b c) (l, f)
visibleFrabels f (Pane label frame fill) = uncurry Pane <$> f (label, frame) <*> case fill of
    Branch layout stack ->
        let t = case layout of Stacked -> center
                               _ -> traverse
        in Branch layout <$> (t.content.visibleFrabels) f stack
    leaf -> pure leaf

visibleLabels :: Traversal' (Pane l f b c) l
visibleLabels = visibleFrabels._1

visibleFrames :: Traversal' (Pane l f b c) f
visibleFrames = visibleFrabels._2


visibleLeaves :: Traversal' (Pane l f b c) (Either b c)
visibleLeaves f (Pane label frame fill) = Pane label frame <$> case fill of
    Leaf e -> Leaf <$> f e
    Branch layout stack ->
        let t = case layout of
                Stacked -> center
                _ -> traverse
        in Branch layout <$> (t.content.visibleLeaves) f stack

visibleBlanks :: Traversal' (Pane l f b c) b
visibleBlanks = visibleLeaves._Left

visibleClients :: Traversal' (Pane l f b c) c
visibleClients = visibleLeaves._Right


justVisible :: Pane l f b c -> Pane l f b c
justVisible (Pane layout frame fill) = Pane layout frame $ case fill of
    Branch Stacked (Stack ls x rs) -> Branch Stacked (Stack [] (x & content %~ justVisible) [])
    Branch layout stack -> Branch layout (stack & traverse.content %~ justVisible)
    leaf -> leaf


insertFlat :: (Applicative m, Default l) => m f -> Either b c -> Fill l f b c -> m (Fill l f b c)
insertFlat m e (Leaf e') = go <$> m <*> m
  where
    go fa fb = Branch Horizontal (Stack [Part def (Pane def fa (Leaf e'))] (Part def (Pane def fb (Leaf e))) [])
insertFlat m e (Branch layout stack) = go <$> m
  where
    go f = Branch layout $ insertR (Part def (Pane def f (Leaf e))) stack

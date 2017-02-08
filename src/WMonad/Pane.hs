{-# LANGUAGE TemplateHaskell #-}

module WMonad.Pane
    ( Pane(..)
    , label
    , fill

    , Fill(..)
    , _Leaf
    , _Branch

    , Part(..)
    , size
    , content

    , Layout(..)

    -- * Operations
    , visibleLeaves
    , insertFlat
    , spreadOut
    , tags

    ) where


import WMonad.Stack

import Control.Lens hiding (Empty)
import Data.Default
import Data.Traversable


data Pane n t a = Pane { _label :: t
                       , _fill :: Fill n t a
                       } deriving (Eq, Show, Read)

data Fill n t a = Leaf a | Branch Layout (Stack (Part n t a))
    deriving (Eq, Show, Read)

data Layout = Stacked | Horizontal | Vertical
    deriving (Eq, Show, Read)

data Part n t a = Part { _size :: n
                       , _content :: Pane n t a
                       } deriving (Eq, Show, Read)

$(makeLenses ''Pane)
$(makeLenses ''Part)
$(makePrisms ''Fill)


instance Traversable (Pane n t) where
    traverse f (Pane t fill) = Pane t <$> traverse f fill

instance Functor (Pane n t) where fmap = fmapDefault
instance Foldable (Pane n t) where foldMap = foldMapDefault


instance Traversable (Fill n t) where
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Branch l stack) = Branch l <$> (traverse.traverse) f stack

instance Functor (Fill n b) where fmap = fmapDefault
instance Foldable (Fill n b) where foldMap = foldMapDefault


instance Traversable (Part n t) where
    traverse f (Part n pane) = Part n <$> traverse f pane

instance Functor (Part n t) where fmap = fmapDefault
instance Foldable (Part n t) where foldMap = foldMapDefault


insertFlat :: (Default n, Default t) => a -> Fill n t a -> Fill n t a
insertFlat a (Leaf a') = Branch Horizontal $ Stack [Part def (Pane def (Leaf a'))] (Part def (Pane def (Leaf a))) []
insertFlat a (Branch l stack) = Branch l . spreadOut $ insertR (Part def (Pane def (Leaf a))) stack


spreadOut :: Default n => Stack (Part n t a) -> Stack (Part n t a)
spreadOut = traverse.size .~ def


tags :: Traversal (Pane n t a) (Pane n t' a) t t'
tags f (Pane t fill) = Pane <$> f t <*> go f fill
  where
    go f (Leaf a) = pure (Leaf a)
    go f (Branch l stack) = Branch l <$> (traverse.content.tags) f stack


visibleLeaves :: Traversal' (Pane n t a) a
visibleLeaves f (Pane t fill) = Pane t <$> case fill of
    Leaf a ->  Leaf <$> f a
    Branch l stack -> let trav = case l of
                            Stacked -> center
                            _ -> traverse
                      in Branch Stacked <$> (trav.content.visibleLeaves) f stack

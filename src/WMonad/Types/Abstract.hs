module WMonad.Types.Abstract
    ( Stack(..)
    , Pane(..)
    , Fill(..)
    , Part(..)
    , Layout(..)
    , PaneSet(..)
    , Screen(..)
    , Workspace(..)
    , RationalRect(..)
    ) where


import Data.Traversable
import qualified Data.Map as M
import qualified Data.Set as S


data Stack a = Stack [a] a [a]
    deriving (Eq, Show, Read)

instance Traversable Stack where
    traverse f (Stack ls x rs) = Stack <$> traverse f ls <*> f x <*> traverse f rs

instance Functor Stack where fmap = fmapDefault
instance Foldable Stack where foldMap = foldMapDefault


data Pane n t a = Pane { _label :: t
                       , _fill :: Fill n t a
                       } deriving (Eq, Show, Read)

data Fill n t a = Empty | Leaf a | Branch Layout (Stack (Part n t a))
    deriving (Eq, Show, Read)

data Layout = Stacked | Horizontal | Vertical
    deriving (Eq, Show, Read)

data Part n t a = Part { _size :: n
                       , _content :: Pane n t a
                       } deriving (Eq, Show, Read)

--

data PaneSet sid sd i n t a = TreeSet
    { _current :: Screen sid sd i n t a
    , _visible :: [Screen sid sd i n t a]
    , _hidden :: [Workspace i n t a]
    , _floating :: M.Map a RationalRect
    } deriving (Eq, Show, Read)


data Screen sid sd i n t a = Screen
    { _screen :: sid
    , _screenDetail :: sd
    , _workspace :: Workspace i n t a
    } deriving (Eq, Show, Read)


data Workspace i n t a = Workspace
    { _tag :: i
    , _pane :: Pane n t a
    } deriving (Eq, Show, Read)


data RationalRect = RationalRect Rational Rational Rational Rational
    deriving (Eq, Show, Read)


-- Instances

instance Traversable (Pane n t) where
    traverse f (Pane t fill) = Pane t <$> traverse f fill

instance Functor (Pane n t) where fmap = fmapDefault
instance Foldable (Pane n t) where foldMap = foldMapDefault


instance Traversable (Fill n t) where
    traverse f Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Branch l stack) = Branch l <$> (traverse.traverse) f stack

instance Functor (Fill n b) where fmap = fmapDefault
instance Foldable (Fill n b) where foldMap = foldMapDefault


instance Traversable (Part n t) where
    traverse f (Part n pane) = Part n <$> traverse f pane

instance Functor (Part n t) where fmap = fmapDefault
instance Foldable (Part n t) where foldMap = foldMapDefault

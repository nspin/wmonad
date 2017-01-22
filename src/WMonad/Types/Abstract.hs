module WMonad.Types.Abstract
    ( Stack(..)
    , Layout(..)
    , Pane(..)
    , Tree(..)
    , ABranch(..)
    , TreeSet(..)
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

data Layout = Stacked | Horizontal | Vertical deriving (Eq, Show, Read)

data Pane n b l = Pane { _size :: n
                       , _content :: Tree n b l
                       } deriving (Eq, Show, Read)

data Tree n b l = Leaf l | Branch Layout b (Stack (Pane n b l)) deriving (Eq, Show, Read)

data ABranch n b l = ABranch { _aLayout :: Layout
                             , _aRoot :: b
                             , _aStack :: (Stack (Pane n b l))
                             } deriving (Eq, Show, Read)


instance Traversable (Pane n b) where
    traverse f (Pane s t) = Pane s <$> traverse f t

instance Functor (Pane n b) where fmap = fmapDefault
instance Foldable (Pane n b) where foldMap = foldMapDefault


instance Traversable (Tree n b) where
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Branch l a stack) = Branch l a <$> traverse (traverse f) stack

instance Functor (Tree n b) where fmap = fmapDefault
instance Foldable (Tree n b) where foldMap = foldMapDefault

data TreeSet i n b l sid sd = TreeSet
    { _current :: Screen i n b l sid sd
    , _visible :: [Screen i n b l sid sd]
    , _hidden :: [Workspace i n b l]
    , _floating :: M.Map l RationalRect
    } deriving (Eq, Show, Read)


data Screen i n b l sid sd = Screen
    { _workspace :: Workspace i n b l
    , _screen :: sid
    , _screenDetail :: sd
    } deriving (Eq, Show, Read)


data Workspace i n b l = Workspace
    { _tag :: i
    , _tree :: Maybe (Tree n b l)
    } deriving (Eq, Show, Read)


data RationalRect = RationalRect Rational Rational Rational Rational
    deriving (Eq, Show, Read)

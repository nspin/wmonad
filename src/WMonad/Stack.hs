module WMonad.Stack
    ( Stack(..)

    -- * Operations
    , center
    , right
    , left
    , singleton
    , fromFocusList
    , toFocusList
    , insertR
    , insertL
    , removeR
    , removeL
    , discard

    ) where


import Control.Lens hiding (Empty)
import Data.Traversable


data Stack a = Stack [a] a [a]
    deriving (Eq, Show, Read)


instance Traversable Stack where
    traverse f (Stack ls x rs) = Stack <$> traverse f ls <*> f x <*> traverse f rs

instance Functor Stack where fmap = fmapDefault
instance Foldable Stack where foldMap = foldMapDefault


-- | ... a [b] c ... => b
center :: Lens' (Stack a) a
center f (Stack ls x rs) = flip (Stack ls) rs <$> f x

-- | ... a [b] c ... => ... a b [c] ...
right :: Stack a -> Stack a
right (Stack ls x []) = let r:rs = reverse (x:ls) in Stack [] r rs
right (Stack ls x (r:rs)) = Stack (x:ls) r rs

-- | ... a [b] c ... => ... [a] b c ...
left :: Stack a -> Stack a
left (Stack [] x rs) = let l:ls = reverse (x:rs) in Stack ls l []
left (Stack (l:ls) x rs) = Stack ls l (x:rs)


singleton :: a -> Stack a
singleton x = Stack [] x []

fromFocusList :: a -> [a] -> Stack a
fromFocusList = Stack []

toFocusList :: Stack a -> (a, [a])
toFocusList (Stack ls x rs) = (x, rs ++ reverse ls)


-- | ... a [b] c ... => ... a b [x] c ...
insertR :: a -> Stack a -> Stack a
insertR x (Stack ls l rs) = Stack (l:ls) x rs

-- | ... a [b] c ... => ... a [x] b c ...
insertL :: a -> Stack a -> Stack a
insertL x (Stack ls r rs) = Stack ls x (r:rs)


-- | ... a [b] c ... => ... a [c] ...
removeR :: Stack a -> Maybe (a, Stack a)
removeR (Stack _ _ []) = Nothing
removeR (Stack ls x (r:rs)) = Just (x, Stack ls r rs)

-- | ... a [b] c ... => ... [a] c ...
removeL :: Stack a -> Maybe (a, Stack a)
removeL (Stack [] _ _) = Nothing
removeL (Stack (l:ls) x rs) = Just (x, Stack ls l rs)


-- | ... a [b] c ... => ... a c [b] ...
swapR :: Stack a -> Stack a
swapR (Stack ls x (r:rs)) = Stack (r:ls) x rs
swapR (Stack ls x []) = Stack [] x (reverse ls)

-- | ... a [b] c ... => ... [b] a c ...
swapL :: Stack a -> Stack a
swapL (Stack (l:ls) x (r:rs)) = Stack (r:ls) x rs
swapL (Stack ls x []) = Stack [] x (reverse ls)


-- | ... a [b] c ... => [b]
discard :: Stack a -> Stack a
discard (Stack _ x _) = Stack [] x []

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module WMonad.Types.Abstract.Operations
    ( module WMonad.Types.Abstract.Operations.Gen

    -- * Stack
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

    -- * Pane
    , mapPaneMaybe
    , filterPane
    , visibleLeaves
    , insertFlat
    , spreadOut
    , tags

    -- * PaneSet
    , workspaces
    , visibleScreens
    , visibleWorkspaces
    , workspacesContaining
    , findTag
    , allClients

    ) where


import WMonad.Types.Abstract
import WMonad.Types.Abstract.Operations.Gen

import Control.Lens hiding (Empty)
import Data.Default


insertFlat :: (Default n, Default t) => a -> Fill n t a -> Fill n t a
insertFlat a Empty = Leaf a
insertFlat a (Leaf a') = Branch Horizontal $ Stack [Part def (Pane def (Leaf a'))] (Part def (Pane def (Leaf a))) []
insertFlat a (Branch l stack) = Branch l . spreadOut $ insertR (Part def (Pane def (Leaf a))) stack


spreadOut :: Default n => Stack (Part n t a) -> Stack (Part n t a)
spreadOut = traverse.size .~ def


tags :: Traversal (Pane n t a) (Pane n t' a) t t'
tags f (Pane t fill) = Pane <$> f t <*> go f fill
  where
    go f Empty = pure Empty
    go f (Leaf a) = pure (Leaf a)
    go f (Branch l stack) = Branch l <$> (traverse.content.tags) f stack


-- Stack

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


-- Pane

visibleLeaves :: Traversal' (Pane n t a) a
visibleLeaves f (Pane t fill) = Pane t <$> case fill of
    Empty -> pure Empty
    Leaf a ->  Leaf <$> f a
    Branch l stack -> let trav = case l of
                            Stacked -> center
                            _ -> traverse
                      in Branch Stacked <$> (trav.content.visibleLeaves) f stack


mapPaneMaybe :: (a -> Maybe b) -> Pane n t a -> Pane n t b
mapPaneMaybe f (Pane t fill) = Pane t $ case fill of
    Empty -> Empty
    Leaf a -> maybe Empty Leaf $ f a
    Branch l stack -> Branch l $ stack & traverse.content %~ mapPaneMaybe f


filterPane :: (a -> Bool) -> Pane n t a -> Pane n t a
filterPane p = mapPaneMaybe $ \x -> if p x then Just x else Nothing


-- PaneSet

workspaces :: Traversal' (PaneSet sid sd i n t a) (Workspace i n t a)
workspaces f PaneSet{..} = PaneSet
    <$> traverseOf workspace f _current
    <*> traverseOf (traverse.workspace) f _visible
    <*> traverseOf traverse f _hidden
    <*> pure _floating


visibleScreens :: Traversal' (PaneSet sid sd i n t a) (Screen sid sd i n t a)
visibleScreens f PaneSet{..} = PaneSet
    <$> f _current
    <*> traverse f _visible
    <*> pure _hidden
    <*> pure _floating


visibleWorkspaces :: Traversal' (PaneSet sid sd i n t a) (Workspace i n t a)
visibleWorkspaces = visibleScreens.workspace


workspacesContaining :: Eq a => a -> Traversal' (PaneSet sid sd i n t a) (Workspace i n t a)
workspacesContaining a f (PaneSet c v h fl) = PaneSet
    <$> (workspace.one) f c
    <*> (traverse.workspace.one) f v
    <*> (traverse.one) f h
    <*> pure fl
  where
    one g ws = if elemOf (traverse) a ws then g ws else pure ws


findTag :: Eq a => a -> Traversal' (PaneSet sid sd i n t a) i
findTag a = workspacesContaining a . tag


allClients :: Fold (PaneSet sid sd i n t a) a
allClients f ps@PaneSet{..} = PaneSet
    <$> traverse f _current
    <*> (traverse.traverse) f _visible
    <*> (traverse.traverse) f _hidden
    <*> pure _floating

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WMonad.Geometry
    ( layout
    ) where


import WMonad.Types

import Graphics.XHB

import Control.Lens hiding (Empty)


layout :: Integral n => RECTANGLE -> Pane n t a -> ([(Maybe RECTANGLE, t)], [(Maybe RECTANGLE, a)])
layout r p = let c = computeLayout r p in (c^..tags, c^..traverse)


computeLayout :: forall n t a. Integral n => RECTANGLE -> Pane n t a -> Pane n (Maybe RECTANGLE, t) (Maybe RECTANGLE, a)
computeLayout r (Pane t fill) = Pane (Just r, t) $ case fill of
    Empty -> Empty
    Leaf a -> Leaf (Just r', a)
    Branch Stacked (Stack ls (Part n pane) rs) -> Branch Stacked $ Stack (no ls) (Part n (computeLayout r' pane)) (no rs)
    Branch Vertical parts -> Branch Vertical (vert parts)
    Branch Horizontal parts -> Branch Horizontal (horiz parts)
  where
    r'@MkRECTANGLE{..} = shrinkBy 1 r
    -- no :: [Part n t a] -> [Part n (Maybe RECTANGLE, t) (Maybe RECTANGLE, a)]
    -- no = traverse.content %~ (tags %~ (,) Nothing).(traverse %~ (,) Nothing)
    no = undefined
    vert parts = undefined
    horiz parts = undefined


shrinkBy :: Integral n => n -> RECTANGLE -> RECTANGLE
shrinkBy n MkRECTANGLE{..} = MkRECTANGLE
    (x_RECTANGLE + fromIntegral n)
    (y_RECTANGLE + fromIntegral n)
    (height_RECTANGLE - 2 * fromIntegral n)
    (width_RECTANGLE - 2 * fromIntegral n)

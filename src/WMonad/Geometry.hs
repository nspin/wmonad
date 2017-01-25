{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WMonad.Geometry
    ( layout
    ) where


import WMonad.Types

import Graphics.XHB

import Control.Lens hiding (Empty)
import Control.Monad.State


layout :: (Num n, RealFrac n) => RECTANGLE -> Pane n t a -> ([(Maybe RECTANGLE, t)], [(Maybe RECTANGLE, a)])
layout r p = let c = solveLayout r p in (c^..tags, c^..traverse)


solveLayout :: (Num n, RealFrac n) => RECTANGLE -> Pane n t a -> Pane n (Maybe RECTANGLE, t) (Maybe RECTANGLE, a)
solveLayout r (Pane t fill) = Pane (Just r, t) $ case fill of
    Empty -> Empty
    Leaf a -> Leaf (Just r', a)
    Branch Stacked (Stack ls (Part n pane) rs) -> Branch Stacked $ Stack (no ls) (Part n (solveLayout r' pane)) (no rs)
    Branch Vertical parts -> Branch Vertical (distributeStack False r' parts)
    Branch Horizontal parts -> Branch Horizontal (distributeStack True r' parts)
  where
    r'@MkRECTANGLE{..} = shrinkBy 1 r
    no = traverse.content %~ (tags %~ (,) Nothing).(traverse %~ (,) Nothing)


distributeStack :: forall n t a. (Num n, RealFrac n) => Bool -> RECTANGLE -> Stack (Part n t a) -> Stack (Part n (Maybe RECTANGLE, t) (Maybe RECTANGLE, a))
distributeStack isHorizontal r@MkRECTANGLE{..} parts@(Stack ls x rs) = evalState (traverse ration parts) (False, 0)
  where
    total :: Integer
    total = fromIntegral $ if isHorizontal then width_RECTANGLE else height_RECTANGLE

    mkRect soFar amnt = if isHorizontal
        then r { x_RECTANGLE = fromIntegral soFar + x_RECTANGLE
               , width_RECTANGLE = fromIntegral amnt
               }
        else r { y_RECTANGLE = fromIntegral soFar + y_RECTANGLE
               , height_RECTANGLE = fromIntegral amnt
               }

    scale' theSize = floor (toRational theSize * toRational total / toRational (sumOf (traverse.size) parts))
    scaledTotal = sum . map scale' $ parts^..traverse.size
    leftOver = total - scaledTotal
    scale isFirst theSize = scale' theSize + (if isFirst then leftOver else 0)

    ration :: Part n t a -> State (Bool, Integer) (Part n (Maybe RECTANGLE, t) (Maybe RECTANGLE, a))
    ration part@Part{..} = state $ \(isFirst, soFar) ->
        ( part & content %~ solveLayout (mkRect soFar (scale isFirst _size))
        , (False, soFar + scale isFirst _size)
        )


shrinkBy :: Integral n => n -> RECTANGLE -> RECTANGLE
shrinkBy n MkRECTANGLE{..} = MkRECTANGLE
    (x_RECTANGLE + fromIntegral n)
    (y_RECTANGLE + fromIntegral n)
    (height_RECTANGLE - 2 * fromIntegral n)
    (width_RECTANGLE - 2 * fromIntegral n)

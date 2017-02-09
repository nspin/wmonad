{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WMonad.Geometry
    ( solveLayout

    -- * Useful Operations
    , eithered
    , nothingPair
    , justPair
    , partitionMaybes
    , partitionEithers
    ) where


import WMonad.Stack
import WMonad.Pane
import WMonad.Util.X

import Graphics.XHB
import Graphics.XHB.Gen.Xinerama

import Data.Bifunctor
import Data.Either
import Control.Lens
import Control.Monad.State


eithered :: Iso' (Maybe a, b) (Either b (a, b))
eithered = iso f g
  where
    f (Nothing, b) = Left b
    f (Just a, b) = Right (a, b)
    g (Left b) = (Nothing, b)
    g (Right (a, b)) = (Just a, b)

nothingPair :: Traversal' (Maybe a, b) b
nothingPair = eithered._Left

justPair :: Traversal' (Maybe a, b) (a, b)
justPair = eithered._Right

partitionMaybes :: [(Maybe a, b)] -> ([b], [(a, b)])
partitionMaybes = partitionEithers . toListOf (traverse.eithered)

--

solveLayout :: RECTANGLE
            -> Pane l f b c
            -> Pane l (Maybe RECTANGLE, f) (Maybe RECTANGLE, b) (Maybe RECTANGLE, c)

solveLayout r (Pane label frame fill) = Pane label (Just r, frame) $ case fill of
    Leaf a -> Leaf $ bimap ((,) (Just r')) ((,) (Just r')) a 
    Branch Stacked (Stack ls (Part n pane) rs) -> Branch Stacked $ Stack (no ls) (Part n (solveLayout r' pane)) (no rs)
    Branch Vertical parts -> Branch Vertical (distributeStack False r' parts)
    Branch Horizontal parts -> Branch Horizontal (distributeStack True r' parts)
  where
    r'@MkRECTANGLE{..} = shrinkBy 1 r
    no = traverse.content %~ (frames %~ (,) Nothing).(blanks %~ (,) Nothing).(clients %~ (,) Nothing)


distributeStack :: forall l f b c. Bool
                                -> RECTANGLE
                                -> Stack (Part l f b c)
                                -> Stack (Part l (Maybe RECTANGLE, f) (Maybe RECTANGLE, b) (Maybe RECTANGLE, c))

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

    scale' theSize = floor (toRational theSize * toRational total / toRational (sumOf (traverse.portion) parts))
    scaledTotal = sum . map scale' $ parts^..traverse.portion
    leftOver = total - scaledTotal
    scale isFirst theSize = scale' theSize + (if isFirst then leftOver else 0)

    ration :: Part l f b c -> State (Bool, Integer) (Part l (Maybe RECTANGLE, f) (Maybe RECTANGLE, b) (Maybe RECTANGLE, c))
    ration part@Part{..} = state $ \(isFirst, soFar) ->
        ( part & content %~ solveLayout (mkRect soFar (scale isFirst _portion))
        , (False, soFar + scale isFirst _portion)
        )


shrinkBy :: Integral n => n -> RECTANGLE -> RECTANGLE
shrinkBy n MkRECTANGLE{..} = MkRECTANGLE
    (x_RECTANGLE + fromIntegral n)
    (y_RECTANGLE + fromIntegral n)
    (width_RECTANGLE - 2 * fromIntegral n)
    (height_RECTANGLE - 2 * fromIntegral n)

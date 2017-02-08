module WMonad.Pane.Draw
    ( draw
    ) where


import WMonad.Pane
import WMonad.Stack

import Data.Function


type Chunk = [String]


draw :: (Show n, Show t, Show a) => Pane n t a -> [String]
draw (Pane t fill) = ("{" ++ show t ++ ": " ++ this ++ "}"):that
  where
    (this, that) = case fill of
        Leaf a -> ("LEAF " ++ show a, [])
        Branch l (Stack ls (Part s x) rs) -> (withLayout l t, ["|"] ++ foldl1 joinSibblings (f ls ++ [c] ++ f rs))
          where
            c = ["(" ++ show s ++ ")", "|"] ++ draw x
            f = map $ \(Part s' x') -> [show s', "|"] ++ draw x'


width :: Chunk -> Int
width = maximum . map length

withLayout :: Show a => Layout -> a -> String
withLayout Stacked a = "[" ++ show a ++ "]"
withLayout Vertical a = "=" ++ show a ++ "="
withLayout Horizontal a = "|" ++ show a ++ "|"

padTo :: Int -> a -> [a] -> [a]
padTo n c s = take n (s ++ repeat c)

joinSibblings :: Chunk -> Chunk -> Chunk
joinSibblings x@(a:as) (b:bs) = (padTo left '-' a ++ b) : (zipWith (++) `on` padTo height "") bleft bs
  where
    left = width x + 1
    bleft = (map (padTo left ' ') as)
    height = (max `on` length) bleft bs

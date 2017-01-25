module Scratch
    ( main
    ) where


import WMonad.Geometry
import WMonad.Types
import WMonad.Types.Abstract.Draw

import Control.Lens
import Control.Monad.State


getNext :: State Int Int
getNext = state $ \c -> (c + 1, c + 1)

test :: Int -> State Int (Pane Rational Int String)
test 0 = do
    n <- getNext
    return $ Pane n (Leaf (show n))
test n = do
    a <- getNext
    b <- getNext
    x <- test (n - 1)
    y <- test (n - 1)
    z <- test (n - 1)
    return . Pane a . Branch Stacked $ Stack
                [Part 4 x]
                (Part 5 (Pane b (Leaf (show b))))
                [Part 7 y, Part 2 z]

main :: IO ()
main = mapM_ putStrLn . draw $ evalState (test 2) 100

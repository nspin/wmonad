module WMonad.Util.X
    ( selectInput
    , getCleanedScreenInfo
    -- * Constants
    , noneId
    , currentTime
    , anyKey
    ) where


import Data.List

import Graphics.XHB
import Graphics.XHB.Gen.Xinerama
import Graphics.XHB.Monad


noneId :: XidLike id => id
noneId = fromXid xidNone

currentTime :: TIMESTAMP
currentTime = noneId

anyKey :: KEYCODE
anyKey = 0


selectInput :: MonadX x m => WINDOW -> [EventMask] -> m ()
selectInput w ems = notify $ MkChangeWindowAttributes w vp
  where
    vp = toValueParam [(CWEventMask, toMask ems)]


getCleanedScreenInfo :: MonadX x m => m [ScreenInfo]
getCleanedScreenInfo = do
    MkQueryScreensReply _ sc <- req MkQueryScreens
    return $ nubScreens sc

nubScreens :: [ScreenInfo] -> [ScreenInfo]
nubScreens xs = nub . filter (\x -> not $ any (x `containedIn`) xs) $ xs

containedIn :: ScreenInfo -> ScreenInfo -> Bool
containedIn r1@(MkScreenInfo x1 y1 w1 h1) r2@(MkScreenInfo x2 y2 w2 h2)
 = and [ r1 /= r2
       , x1 >= x2
       , y1 >= y2
       , fromIntegral x1 + w1 <= fromIntegral x2 + w2
       , fromIntegral y1 + h1 <= fromIntegral y2 + h2
       ]

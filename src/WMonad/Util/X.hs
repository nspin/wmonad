{-# LANGUAGE RecordWildCards #-}

module WMonad.Util.X
    ( getCleanedScreenInfo
    , keyButToMod
    , screenInfoToRect
    -- * Constants
    , noneId
    , currentTime
    , anyKey
    -- * Events
    , clientMask
    , rootMask
    -- * Server Actions
    , selectInput
    , tileWindow
    , getBorderWidth
    , setWindowBorder
    , setWindowBorderWidth

    , nubScreens
    ) where


import Data.List
import Data.Word

import Graphics.XHB
import Graphics.XHB.Gen.Xinerama
import Graphics.XHB.Monad


noneId :: XidLike id => id
noneId = fromXid xidNone

currentTime :: TIMESTAMP
currentTime = noneId

anyKey :: KEYCODE
anyKey = 0


getScreenInfo :: MonadX x m => Setup -> m [RECTANGLE]
getScreenInfo setup = do
    MkIsActiveReply ok <- req MkIsActive
    if ok /= 0
        then map screenInfoToRect . screen_info_QueryScreensReply <$> req MkQueryScreens
        else return $
            let MkSCREEN{..} = head (roots_Setup setup)
            in [MkRECTANGLE 0 0 width_in_pixels_SCREEN height_in_pixels_SCREEN]
            

getCleanedScreenInfo :: MonadX x m => Setup -> m [RECTANGLE]
getCleanedScreenInfo = fmap nubScreens . getScreenInfo

nubScreens :: [RECTANGLE] -> [RECTANGLE]
nubScreens xs = nub $ filter (\x -> not $ any (x `containedIn`) xs) xs

containedIn :: RECTANGLE -> RECTANGLE -> Bool
containedIn r1@(MkRECTANGLE x1 y1 w1 h1) r2@(MkRECTANGLE x2 y2 w2 h2) = and
    [ r1 /= r2
    , x1 >= x2
    , y1 >= y2
    , fromIntegral x1 + w1 <= fromIntegral x2 + w2
    , fromIntegral y1 + h1 <= fromIntegral y2 + h2
    ]


clientMask :: [EventMask]
clientMask = [ EventMaskStructureNotify
             , EventMaskEnterWindow
             , EventMaskPropertyChange
             ]

rootMask :: [EventMask]
rootMask = [ EventMaskSubstructureRedirect
           , EventMaskSubstructureNotify
           , EventMaskEnterWindow
           , EventMaskLeaveWindow
           , EventMaskStructureNotify
           , EventMaskButtonPress
           ]


keyButToMod :: KeyButMask -> Maybe ModMask
keyButToMod KeyButMaskShift   = Just ModMaskShift
keyButToMod KeyButMaskLock    = Just ModMaskLock
keyButToMod KeyButMaskControl = Just ModMaskControl
keyButToMod KeyButMaskMod1    = Just ModMask1
keyButToMod KeyButMaskMod2    = Just ModMask2
keyButToMod KeyButMaskMod3    = Just ModMask3
keyButToMod KeyButMaskMod4    = Just ModMask4
keyButToMod KeyButMaskMod5    = Just ModMask5
keyButToMod _                 = Nothing

screenInfoToRect :: ScreenInfo -> RECTANGLE
screenInfoToRect (MkScreenInfo x y w h) = MkRECTANGLE x y w h

type Pixel = Word32

selectInput :: MonadX x m => WINDOW -> [EventMask] -> m ()
selectInput w ems = notify $ MkChangeWindowAttributes w vp
  where
    vp = toValueParam [(CWEventMask, toMask ems)]

moveResizeWindow :: MonadX x m => WINDOW -> RECTANGLE -> m ()
moveResizeWindow w MkRECTANGLE{..} = notify $ MkConfigureWindow w vp
  where
    vp = toValueParam [ (ConfigWindowX, fromIntegral x_RECTANGLE)
                      , (ConfigWindowY, fromIntegral y_RECTANGLE)
                      , (ConfigWindowWidth, fromIntegral width_RECTANGLE)
                      , (ConfigWindowHeight, fromIntegral height_RECTANGLE)
                      ]

tileWindow :: MonadX x m => RECTANGLE ->  WINDOW -> m ()
tileWindow MkRECTANGLE{..} w =  do
    bw <- getBorderWidth w
    let least x | x <= bw*2  = 1
                | otherwise  = x - bw*2
    moveResizeWindow w $ MkRECTANGLE
        x_RECTANGLE
        y_RECTANGLE
        (least width_RECTANGLE)
        (least height_RECTANGLE)

getBorderWidth :: (MonadX x m, Integral n) => WINDOW -> m n
getBorderWidth w = (fromIntegral . border_width_GetGeometryReply) <$> req (MkGetGeometry ((fromXid.toXid) w))

setWindowBorder :: MonadX x m => WINDOW -> Pixel -> m ()
setWindowBorder w px = notify $ MkConfigureWindow w vp
  where
    vp = toValueParam [(CWBorderPixel, px)]

setWindowBorderWidth :: MonadX x m => WINDOW -> Word32 -> m ()
setWindowBorderWidth w bw = notify $ MkConfigureWindow w vp
  where
    vp = toValueParam [(ConfigWindowBorderWidth, bw)]

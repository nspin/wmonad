module WMonad.Util
    ( whenM
    , logs
    ) where


import Control.Monad
import Control.Monad.Logger
import Data.Text (pack)


whenM :: Monad m => m Bool -> m () -> m ()
whenM cond m = cond >>= flip when m

logs :: (Show a, MonadLogger m) => a -> m ()
logs = logDebugN . pack . show

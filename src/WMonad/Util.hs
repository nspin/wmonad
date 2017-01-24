module WMonad.Util
    ( whenM
    , unlessM
    , logp
    , logs
    ) where


import Control.Monad
import Control.Monad.Logger
import Data.Text (pack)


whenM :: Monad m => m Bool -> m () -> m ()
whenM cond m = cond >>= flip when m


unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond m = cond >>= flip unless m


logp :: (Show a, MonadLogger m) => a -> m ()
logp = logDebugN . pack . show

logs :: MonadLogger m => String -> m ()
logs = logDebugN . pack . show

{-# LANGUAGE TemplateHaskell #-}

module WMonad.Types.Concrete.Operations.Gen where

import WMonad.Types.Concrete
import Control.Lens

-- There are name conflicts with makeClassy,
-- and exporting all lenses by name would be annoying

$(makeLenses ''WState)
$(makeLenses ''WEnv)
$(makeLenses ''Config)

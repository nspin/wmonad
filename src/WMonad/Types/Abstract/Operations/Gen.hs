{-# LANGUAGE TemplateHaskell #-}

module WMonad.Types.Abstract.Operations.Gen where

import WMonad.Types.Abstract
import Control.Lens

-- There are name conflicts with makeClassy,
-- and exporting all lenses by name would be annoying

$(makeLenses ''Pane)
$(makeLenses ''Part)
$(makeLenses ''PaneSet)
$(makeLenses ''Screen)
$(makeLenses ''Workspace)
$(makePrisms ''Fill)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy     #-}

-- | This module defines a ETCS /Speed and Distance Monitoring/ subsystem.
module ETCS.SDM where

--import           Control.Lens                         hiding ((*~), _1, _2)
import           ETCS.SDM.BreakingModel

import           FRP.Sodium
--import           Numeric.Units.Dimensional.TF.Prelude
--import           Prelude                              ()




sdm :: Behavior (A_Break Double)
sdm = undefined



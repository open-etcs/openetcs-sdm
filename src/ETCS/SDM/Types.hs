

module ETCS.SDM.Types where

import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()

data BreakPosition = FreightTrainG | PassangerTrainP | FreightTrainP

type BreakingPercentage f =  Dimensionless f

type A_Break f = Velocity f -> Acceleration f

type T_Break f = Velocity f -> Time f






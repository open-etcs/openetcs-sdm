{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module ETCS.SDM.Types where

import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()

data BreakPosition = FreightTrainG | PassangerTrainP | FreightTrainP

type BreakingPercentage f =  Dimensionless f

type A_Break f = Velocity f -> Acceleration f

type T_Break f = Velocity f -> Time f


class (Floating f, RealFloat f) => HasBreakingModel (t :: * -> *) f where
  a_break_emergency :: t f -> A_Break f
  a_break_service   :: t f -> A_Break f
  t_break_emergency :: t f -> T_Break f
  t_break_service   :: t f -> T_Break f


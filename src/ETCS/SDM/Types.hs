{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module ETCS.SDM.Types where

import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()


-- | The brake position defines the behaviour of the brake
--   for specific train types.
data BreakPosition = FreightTrainG | PassangerTrainP | FreightTrainP
                   deriving (Show, Read, Eq, Enum, Bounded, Ord)


-- | If the brake percentage is captured as Train Data and the
--   conversion model is applicable, they are used to derive
--   a 'HasBreakingModel'. See 'breakingModelConverter' for details.
type BreakingPercentage f = Dimensionless f

-- | The deceleration due to braking shall be given as
--   a step function of the speed.
type A_Break f = Velocity f -> Acceleration f

-- | The deceleration 'A_brake' is not available immediately after the on-board
--   commands the brake. There is a time lag between brake command and
--   the start of the brake force build-up. There is also time needed to
--   build up the full brake force.
type T_Break f = Velocity f -> Time f


-- | A breaking Model which defined the 'A_Break' and 'T_Break' for
--   emergency and service break.
class (Floating f, RealFloat f) => HasBreakingModel (t :: * -> *) f where
  a_break_emergency :: t f -> A_Break f
  a_break_service   :: t f -> A_Break f
  t_break_emergency :: t f -> T_Break f
  t_break_service   :: t f -> T_Break f


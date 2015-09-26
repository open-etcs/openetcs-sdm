{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module ETCS.SDM.Types where

import           Control.Lens
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
class (Floating f, RealFloat f) => HasBreakingModelBase (t :: * -> *) f where
  a_break_emergency :: t f -> A_Break f
  a_break_service   :: t f -> A_Break f
  t_break_emergency :: t f -> T_Break f
  t_break_service   :: t f -> T_Break f



newtype BreakingModelBase f =
  BreakingModelBase (A_Break f, A_Break f, T_Break f, T_Break f)


data BreakingModel f =
  BreakingModel {
    breakingModelBase :: BreakingModelBase f,
    a_normal_service  :: A_Break f
    }

data ConvertingBreakingModelInput f =
  ConvertingBreakingModelInput {
    _bmiMaxVelocity        :: Velocity f,
    _bmiBreakingPercentage :: BreakingPercentage f,
    _bmiTrainLength        :: Length f,
    _bmiBreakPosition      :: BreakPosition
    }



data ConvertedBreakingModel f =
  ConvertedBreakingModel {
    _cbmBreakingModelInput :: ConvertingBreakingModelInput f,
    _cbmBreakingModel      :: BreakingModelBase f
    }


makePrisms ''BreakPosition

makeClassy ''ConvertingBreakingModelInput

makeLenses ''ConvertedBreakingModel




instance (Floating f, RealFloat f) => HasBreakingModelBase BreakingModel f where
  a_break_emergency = a_break_emergency . breakingModelBase
  a_break_service = a_break_service . breakingModelBase
  t_break_emergency = t_break_emergency . breakingModelBase
  t_break_service = t_break_service . breakingModelBase



instance (Floating f, RealFloat f) => HasBreakingModelBase
         BreakingModelBase f where
  a_break_emergency (BreakingModelBase (a,_,_,_)) = a
  a_break_service   (BreakingModelBase (_,a,_,_)) = a
  t_break_emergency (BreakingModelBase (_,_,a,_)) = a
  t_break_service   (BreakingModelBase (_,_,_,a)) = a


instance (Floating f, RealFloat f) =>
         HasBreakingModelBase ConvertedBreakingModel f where
           a_break_emergency = a_break_emergency . _cbmBreakingModel
           a_break_service = a_break_service . _cbmBreakingModel
           t_break_emergency = t_break_emergency . _cbmBreakingModel
           t_break_service = t_break_service  . _cbmBreakingModel


instance HasConvertingBreakingModelInput (ConvertedBreakingModel f) f where
  convertingBreakingModelInput = cbmBreakingModelInput


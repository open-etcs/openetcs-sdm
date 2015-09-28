{-# LANGUAGE Trustworthy #-}

module ETCS.SDM.Stepper
  ( StepperFunction, mk_a_break_stepper, a_break_stepper
  ) where


import           ETCS.SDM.Types
import           Numeric.Units.Dimensional.TF.Prelude
import qualified Prelude                              as P ()

mk_a_break_stepper :: Ord f => [Velocity f] -> [Acceleration f] ->
                     Maybe (StepperFunction (Velocity f) (Acceleration f))
mk_a_break_stepper as = stepperFunction as

a_break_stepper :: Ord f => StepperFunction (Velocity f) (Acceleration f) ->
                          A_Break f
a_break_stepper = unStepper


newtype StepperFunction a b = StepperFunction {unStepper :: a -> b}

stepperFunction :: Ord a => [a] -> [b] -> Maybe (StepperFunction a b)
stepperFunction as bs
 | ((succ . length $ as) == length bs) && (length bs >= 1) && (length bs <= 7) =
    Just . StepperFunction $ stepperF as bs
 | otherwise = Nothing

stepperF :: Ord a => [a] -> [b] -> a -> b
stepperF [] [b] _ = b
stepperF (a:as) (b:bs) v = if (v <= a) then b else stepperF as bs v
stepperF [] [] _    = error "error evaluating stepperF on [] []"
stepperF [] (_:_) _ = error "error evaluating stepperF on [] (_:_)"
stepperF (_:_) [] _ = error "error evaluating stepperF on (_:_) []"


instance Functor (StepperFunction a) where
  fmap f (StepperFunction sf) = StepperFunction (f . sf)









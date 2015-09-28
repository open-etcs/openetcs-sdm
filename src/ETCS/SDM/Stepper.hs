{-# LANGUAGE Trustworthy #-}

module ETCS.SDM.Stepper
  ( -- * Polymorph Stepper
    StepperFunction, stepperFunction, unStepper
    -- * Break Stepper (v -> a)
  , mk_a_break_stepper, a_break_stepper
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


newtype StepperFunction a b = StepperFunction { unStepper :: a -> b }


stepperFunction :: Ord a => [a] -> [b] -> Maybe (StepperFunction a b)
stepperFunction as bs
 | (length bs == (succ . length $ as)) &&
   (length bs >= 1) &&
   (length bs <= 7) &&
   (orderedList as) =
    Just . StepperFunction $ stepperF as bs
 | otherwise = Nothing


orderedList :: Ord a => [a] -> Bool
orderedList [] = True
orderedList (_:[]) = True
orderedList (a:b:xs) = b > a && orderedList (b:xs)



stepperF :: Ord a => [a] -> [b] -> a -> b
stepperF [] [b] _ = b
stepperF (a:as) (b:bs) v = if (v <= a) then b else stepperF as bs v
stepperF [] [] _    = error "error evaluating stepperF on [] []"
stepperF [] (_:_) _ = error "error evaluating stepperF on [] (_:_)"
stepperF (_:_) [] _ = error "error evaluating stepperF on (_:_) []"


instance Functor (StepperFunction a) where
  fmap f (StepperFunction sf) = StepperFunction (f . sf)









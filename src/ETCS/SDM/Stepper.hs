{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module ETCS.SDM.Stepper (Stepper (..), stepperFunction)  where

import           TypeUnary.Vec



newtype Stepper n a b = Stepper (Vec n a, Vec (S n) b)

stepperFunction :: (Eq a, Ord a, Num a) => Stepper n a b -> a -> b
stepperFunction = stepperFunction' . zipStepperFunction


zipStepperFunction :: (Num a) => Stepper n a b -> (Vec (S n) (a,b))
zipStepperFunction (Stepper (as', bs)) = zipV (0 :< as') $ bs

stepperFunction' :: (Eq a, Ord a, Num a) => Vec (S n) (a, b) -> a -> b
stepperFunction' ((0, b0) :< (a1, b1)  :< vs) k =
  if (0 <= k && k <= a1) then b0 else stepperFunction' ((a1,b1) :< vs) k
stepperFunction' ((a0, b0) :< (a1, b1)  :< vs) k =
  if (a0 < k && k <= a1) then b0 else stepperFunction' ((a1,b1) :< vs) k
stepperFunction' ((_, b) :< ZVec) _ = b


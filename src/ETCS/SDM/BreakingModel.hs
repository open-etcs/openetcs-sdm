{-# LANGUAGE Trustworthy #-}

module ETCS.SDM.BreakingModel
       ( BreakPosition (..), BreakingPercentage, A_Break, T_Break
       , HasBreakingModel(..)
       , NormalServiceModel, a_break_normal_service
       , module ETCS.SDM.BreakingModelConverter
       ) where

import           Numeric.Units.Dimensional.TF.Prelude
import           ETCS.SDM.BreakingModelConverter
import           ETCS.SDM.Types
import           ETCS.SDM.Helper




newtype NormalServiceModelT f =
  NormalServiceModel ( Acceleration f
                     , Acceleration f
                     , A_Break f
                     , A_Break f
                     , A_Break f
                     )

type NormalServiceModel f = BreakPosition -> NormalServiceModelT f




a_break_normal_service :: (HasBreakingModel t f) =>
                         BreakPosition -> t f -> NormalServiceModel f -> A_Break f
a_break_normal_service bpos m nsm_f
  |                   (abs0 < a_sb01) = f0
  | (abs0 >= a_sb01) && (abs0 < a_sb12) = f1
  | otherwise                         = f2
  where abs0 = a_break_service m (0 *~ kmh)
        (NormalServiceModel (a_sb01, a_sb12, f0, f1, f2)) = nsm_f bpos
                

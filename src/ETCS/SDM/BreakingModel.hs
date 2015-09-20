module ETCS.SDM.BreakingModel
       ( A_Break, T_Break
       , HasBreakingModel(..)
       , a_break_normal_service
       , module ETCS.SDM.BreakingModelConverter
       ) where

import           ETCS.SDM.BreakingModelConverter
import           ETCS.SDM.Types





a_break_normal_service :: (HasBreakingModel t f) => t f -> A_Break f
a_break_normal_service m = undefined

{-# LANGUAGE Trustworthy #-}

module ETCS.SDM.BreakingModel
       ( BreakPosition (..), BreakingPercentage, A_Break, T_Break
       , HasBreakingModelBase(..), a_normal_service
       , NormalServiceModel, a_break_normal_service
       , module ETCS.SDM.BreakingModelConverter
       ) where

import           Control.Lens hiding ((*~))
import           ETCS.SDM.BreakingModelConverter
import ETCS.SDM.Helper
import           ETCS.SDM.Types
import Numeric.Units.Dimensional.TF.Prelude




a_break_normal_service ::
  (HasBreakingModelBase t f, RealFloat f, Floating f) =>
  t f -> NormalServiceModels f -> BreakPosition -> Maybe (A_Break f)
a_break_normal_service model nsms bpos =
  let nsmM = nsms ^. case bpos of
        FreightTrainG -> nsms_breakposition_G
        _             -> nsms_breakposition_T
      abs0 = a_break_service model (0 *~ kmh)
      a_break_normal_service' nsm
        | (abs0 < nsm ^. nsm_sb01) =
            nsm ^. nsm_model0
        | (abs0 >= nsm ^. nsm_sb01) && (abs0 < nsm ^. nsm_sb12) =
            nsm ^. nsm_model1
        | otherwise =
            nsm ^. nsm_model2
  in maybe Nothing (Just . a_break_normal_service') nsmM

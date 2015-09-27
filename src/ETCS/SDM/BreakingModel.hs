{-# LANGUAGE Trustworthy #-}

module ETCS.SDM.BreakingModel
       ( A_Break, T_Break
       , NormalServiceModels
       , BreakingModelBase
       , ConvertingBreakingModelInput
       , BreakingModel
       , BreakPosition(..)       
       , mkBreakingModel
       , a_break_emergency, t_break_emergency
       , a_break_service, t_break_service
       , a_break_normal_service
       ) where

import           Control.Lens hiding ((*~))
import           ETCS.SDM.BreakingModelConverter
import ETCS.SDM.Helper
import           ETCS.SDM.Types
import Numeric.Units.Dimensional.TF.Prelude


mkBreakingModel :: (RealFloat f, Floating f) =>
      NormalServiceModels f -> BreakPosition ->
      Either (BreakingModelBase f) (ConvertingBreakingModelInput f) ->      
      Maybe (BreakingModel f)
mkBreakingModel nsm bpos (Left bm) =
  mkBreakingModel' nsm bpos bm
mkBreakingModel nsm bpos (Right i) = 
  preview breakingModelConverter i >>= mkBreakingModel' nsm bpos

mkBreakingModel'
  :: HasBreakingModelBase t f =>
    NormalServiceModels f -> BreakPosition -> t f -> Maybe (BreakingModel f)
mkBreakingModel' nsm bpos bm = do    
  bns <- a_break_normal_service' bm nsm bpos
  return $ BreakingModel {
    breakingModelBase = toBreakingModelBase bm,
    a_break_normal_service = bns
    }

a_break_normal_service' ::
  (HasBreakingModelBase t f, RealFloat f, Floating f) =>
  t f -> NormalServiceModels f -> BreakPosition -> Maybe (A_Break f)
a_break_normal_service' model nsms bpos =
  let nsmM = nsms ^. case bpos of
        FreightTrainG -> nsms_breakposition_G
        _             -> nsms_breakposition_T
      abs0 = a_break_service model (0 *~ kmh)
      a_break_normal_service'' nsm
        | (abs0 < nsm ^. nsm_sb01) =
            nsm ^. nsm_model0
        | (abs0 >= nsm ^. nsm_sb01) && (abs0 < nsm ^. nsm_sb12) =
            nsm ^. nsm_model1
        | otherwise =
            nsm ^. nsm_model2
  in maybe Nothing (Just . a_break_normal_service'') nsmM




    

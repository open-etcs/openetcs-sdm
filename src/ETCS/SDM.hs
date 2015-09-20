{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy     #-}

module ETCS.SDM where

import           Control.Lens                         hiding ((*~), _1, _2)
import           ETCS.SDM.BreakingModelConverter
import           ETCS.SDM.Types
import           FRP.Sodium
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()

a :: (RealFloat f, Floating f) => Prism' (BreakingModelInput f) (ConvertedBreakingModel f)
a = breakingModelConverter

data BreakingModel t =
  BreakingModel {
    _bm_A_break_emergency      :: Behavior (A_Break t),
    _bm_A_break_service        :: Behavior (A_Break t),
    _bm_A_break_normal_service :: Behavior (A_Break t),
    _bm_T_break_service        :: Behavior (T_Break t),
    _bm_T_break_emergency      :: Behavior (T_Break t)
    }

type TracktionModel = ()


data SDMInput t =
  SDMInput {
    _sdmInputBreakPosition :: Behavior BreakPosition,
    _sdmInputBreakingModel :: Behavior (Either (BreakingModel t) (BreakingPercentage t)),
    _sdmInputTractionModel :: Behavior TracktionModel
    }

makeLenses ''SDMInput

data SDM t =
  SDM {
    _sdmBreakingModel :: Behavior (BreakingModel t)
    }

mkSDM :: SDMInput t -> Reactive (SDM t)
mkSDM i = do
  return $ SDM {
    _sdmBreakingModel =
        undefined
    }



sdm :: ()
sdm = ()





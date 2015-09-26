{-# LANGUAGE Trustworthy #-}

module ETCS.SDM.BreakingModelConverter
       ( ConvertedBreakingModel
       , HasBreakingModelInput, BreakingModelInput
       , breakingModelConverter
       ) where

import           Control.Lens
import           ETCS.SDM.Intern
import           ETCS.SDM.Types
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()


breakingModelConverter :: (RealFloat f, Floating f) =>
                         Prism' (BreakingModelInput f) (ConvertedBreakingModel f)
breakingModelConverter =
  prism' (view breakingModelInput) $ \i ->
  if (validConvertion i) then Just . breakingModelConverter' $ i else Nothing

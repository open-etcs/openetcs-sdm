{-# LANGUAGE Trustworthy #-}

module ETCS.SDM.BreakingModelConverter
       ( ConvertedBreakingModel
       , HasConvertingBreakingModelInput, ConvertingBreakingModelInput
       , breakingModelConverter
       ) where

import           Control.Lens
import           ETCS.SDM.Intern
import           ETCS.SDM.Types
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()


breakingModelConverter :: (RealFloat f, Floating f) =>
                         Prism' (ConvertingBreakingModelInput f) (ConvertedBreakingModel f)
breakingModelConverter =
  prism' (view convertingBreakingModelInput) $ \i ->
  if (validConvertion i) then Just . breakingModelConverter' $ i else Nothing

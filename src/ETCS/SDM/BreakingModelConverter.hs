{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE Trustworthy           #-}

module ETCS.SDM.BreakingModelConverter
       ( ConvertedBreakingModel
       , HasBreakingModelInput(..), BreakingModelInput
       , breakingModelConverter
       ) where

import           Control.Lens
import           ETCS.SDM.Intern

import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()


breakingModelConverter :: (RealFloat f, Floating f) =>
                         Prism' (BreakingModelInput f) (ConvertedBreakingModel f)
breakingModelConverter = prism' a b
  where a c = BreakingModelInput {
          _bmiMaxVelocity = c ^. cbmMaxVelocity,
          _bmiBreakingPercentage = c ^. cbmBreakingPercentage,
          _bmiTrainLength = c ^. cbmTrainLength,
          _bmiBreakPosition = c ^. cbmBreakPosition
          }
        b i =
          if (validConvertion i) then Just $ breakingModelConverter' i
          else Nothing

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE Trustworthy           #-}

module ETCS.SDM.BreakingModelConverter
       ( ConvertedBreakingModel
       , BreakingModelInput
       , breakingModelConverter
       ) where

import           Control.Lens.Prism
import           ETCS.SDM.Intern
import           ETCS.SDM.Types
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()

type BreakingModelInput f =
  (Velocity f, BreakingPercentage f, Length f, BreakPosition)


breakingModelConverter :: (RealFloat f, Floating f) =>
                        Prism' (BreakingModelInput f) (ConvertedBreakingModel f)
breakingModelConverter = prism' a b
  where a c = ( _cbmMaxVelocity c
              , _cbmBreakingPercentage c
              , _cbmTrainLength c
              , _cbmBreakPosition c)
        b (vmax, lambda, l, bpos) =
          if (validConvertion vmax lambda l bpos)
          then Just $ breakingModelConverter' vmax lambda l bpos
          else Nothing


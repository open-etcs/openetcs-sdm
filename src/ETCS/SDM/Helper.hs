module ETCS.SDM.Helper where

import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()

kmh :: (Fractional a) => Unit DVelocity a
kmh = (kilo meter) / hour

ms2 :: (Fractional a) => Unit DAcceleration a
ms2 = meter / (second * second)


module ETCS.SDM.Intern where

import           Control.Lens                         hiding ((*~), _2)
import           ETCS.SDM.Helper
import           ETCS.SDM.Types
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              ()


validConvertion :: (HasConvertingBreakingModelInput i f, RealFloat f) => i -> Bool
validConvertion i   =
  let v    = i ^. bmiMaxVelocity
      bp   = i ^. bmiBreakingPercentage
      l    = i ^. bmiTrainLength
      lmax = case (i ^. bmiBreakPosition) of
        PassangerTrainP ->  900 *~ meter
        FreightTrainG   -> 1500 *~ meter
        FreightTrainP   -> 1500 *~ meter
  in (0  *~ kmh   <= v)   && (v  <= 200 *~ kmh) &&
     (30 *~ one   < bp ) && (bp <= 250 *~ one) &&
     (0  *~ meter < l)   && (l  <= lmax)


breakingModelConverter'
  :: (HasConvertingBreakingModelInput i f, RealFloat f, Floating f) => i ->
    ConvertedBreakingModel f
breakingModelConverter' i =
  let (ea, sa) = basicDeceleration $ i ^. bmiBreakingPercentage
      bpos     = i ^. bmiBreakPosition
      l        = i ^. bmiTrainLength
  in ConvertedBreakingModel {
    _cbmBreakingModelInput = i ^. convertingBreakingModelInput,
    _cbmBreakingModel =
      BreakingModelBase (ea, sa, t_brake_emergency_cm bpos l
                        , t_brake_service_cm bpos l)
    }

basicDeceleration :: (RealFloat f, Floating f) =>
                    BreakingPercentage f -> (A_Break f, A_Break f)
basicDeceleration lambda =
  let l0_emergency = lambda
      l0_service = min (135.0 *~ one) lambda
      ad_n l0 v =
        let vlim = v_lim l0
            (a3_n, a2_n, a1_n, a0_n) = a_n_ms n
            n = nfromV vlim v
        in if (v <= vlim) then (ad_0 l0)
           else a3_n * (l0 ** (3 *~ one)) + a2_n * (l0 ** (2 *~ one)) +
                a1_n * l0 + a0_n
  in ( ad_n l0_emergency, ad_n l0_service )


t_brake_service_cm :: (RealFloat f, Floating f) =>
  BreakPosition -> Length f -> Velocity f -> Time f
t_brake_service_cm = t_brake_cm t_brake_basic_sb


t_brake_emergency_cm :: (RealFloat f, Floating f) =>
  BreakPosition -> Length f -> Velocity f -> Time f
t_brake_emergency_cm = t_brake_cm t_brake_basic_eb





ad_0 :: (RealFloat f, Floating f) => BreakingPercentage f -> Acceleration f
ad_0 l0 = a * l0 + b
  where a = 0.0075 *~ ms2
        b = 0.076 *~ ms2

v_lim :: (RealFloat f, Floating f) => BreakingPercentage f -> Velocity f
v_lim l0 = ((l0 ** y)) * x
  where x = 16.85 *~ kmh
        y = 0.428 *~ one


a_n_ms :: (RealFloat f, Floating f) => Int -> ( Acceleration f, Acceleration f
                                          , Acceleration f, Acceleration f)
a_n_ms 1 = ((-6.30e-7) *~ ms2,   6.10e-5  *~ ms2, 4.72e-3 *~ ms2, 0.0663 *~ ms2)
a_n_ms 2 = (  2.73e-7  *~ ms2, (-4.54e-6) *~ ms2, 5.13e-3 *~ ms2, 0.1300 *~ ms2)
a_n_ms 3 = (  5.58e-8  *~ ms2, (-6.76e-6) *~ ms2, 5.81e-3 *~ ms2, 0.0479 *~ ms2)
a_n_ms 4 = (  3.00e-8  *~ ms2, (-3.85e-6) *~ ms2, 5.52e-3 *~ ms2, 0.0480 *~ ms2)
a_n_ms 5 = (  3.23e-9  *~ ms2,   1.66e-6  *~ ms2, 5.06e-3 *~ ms2, 0.0559 *~ ms2)
a_n_ms _ = error "a_n_ms called for undefined n"


nfromV :: (RealFloat f, Floating f) => Velocity f -> Velocity f -> Int
nfromV vlim v
  | ((vlim < v) && (v <= 100 *~ kmh) &&
     (vlim <= 100 *~ kmh)) = 1
  | ((vlim < v) && (v <= 120 *~ kmh) &&
     (100 *~ kmh < vlim) && (vlim <= 120 *~ kmh)) = 2
  | ((100 *~ kmh < v)    && (v <= 120 *~ kmh) &&
     (vlim <= 100 *~ kmh)) = 2
  | ((vlim < v) && (v <= 150 *~ kmh) &&
     (120 *~ kmh < vlim) && (vlim <= 150 *~ kmh)) = 3
  | ((120 *~ kmh < v)    && (v <= 150 *~ kmh) &&
     (vlim <= 120 *~ kmh)) = 3
  | ((vlim < v) && (v <= 180 *~ kmh) &&
     (150 *~ kmh < vlim) && (vlim <= 180 *~ kmh)) = 4
  | ((150 *~ kmh < v)    && (v <= 180 *~ kmh) &&
     (vlim <= 150 *~ kmh)) = 4
  | ((vlim < v)       && (vlim > 180 *~ kmh)) = 5
  | ((180 *~ kmh < v) && (vlim <= 180 *~ kmh)) = 5
  | otherwise = error "nfromV: undefined range"




t_brake_basic :: (RealFloat f, Floating f) =>
  (Length f -> Length f) ->
  Length f -> Time f -> Time f -> Time f -> Time f
t_brake_basic fl l' a b c = a + (b * l) + (c * (l ** _2))
  where l = (fl l') / (100 *~ meter)


t_brake_basic_eb :: (RealFloat f, Floating f) => BreakPosition -> Length f -> Time f
t_brake_basic_eb PassangerTrainP l =
  t_brake_basic (max (400 *~ meter))
  l (2.30 *~ second) (0 *~ second) (0.17 *~ second)
t_brake_basic_eb FreightTrainP l
  | l <= 900 *~ meter =
      t_brake_basic (max (400 *~ meter))
      l (2.30 *~ second) (0 *~ second) (0.17 *~ second)
  | otherwise =
      t_brake_basic (max (400 *~ meter))
      l ((-0.4) *~ second) (1.6 *~ second) (0.03 *~ second)
t_brake_basic_eb FreightTrainG l
  | l <= 900 *~ meter =
       t_brake_basic id
       l (12.0 *~ second) (0 *~ second) (0.05 *~ second)
  | otherwise =
      t_brake_basic id
      l ((-0.4) *~ second) (1.6 *~ second) (0.03 *~ second)

t_brake_basic_sb :: (RealFloat f, Floating f) => BreakPosition -> Length f -> Time f
t_brake_basic_sb PassangerTrainP l =
  t_brake_basic id
  l (3.00 *~ second) (1.5 *~ second) (0.1 *~ second)
t_brake_basic_sb FreightTrainP l
  | l <= 900 *~ meter =
      t_brake_basic id
      l (3 *~ second) (2.77 *~ second) (0 *~ second)
  | otherwise =
      t_brake_basic id
      l (10.5 *~ second) (0.32 *~ second) (0.18 *~ second)
t_brake_basic_sb FreightTrainG l
  | l <= 900 *~ meter =
       t_brake_basic (max (400 *~ meter))
       l (3 *~ second) (2.77 *~ second) (0 *~ second)
  | otherwise =
      t_brake_basic (max (400 *~ meter))
      l (10.5 *~ second) (0.32 *~ second) (0.18 *~ second)



t_brake_cm :: (RealFloat f, Floating f) =>
             (BreakPosition -> Length f -> Time f) ->
             BreakPosition -> Length f -> Velocity f -> Time f
t_brake_cm f bp l v = t_brake_cm' f v bp l

t_brake_cm' :: (RealFloat f, Floating f) =>
  (BreakPosition -> Length f -> Time f) ->
  Velocity f -> BreakPosition -> Length f -> Time f
t_brake_cm' f v_target
  | (v_target == 0 *~ kmh) = f
  | (v_target > 0 *~ kmh) = (\bp l -> f bp l * (kto bp))
  | otherwise = error $ "t_brake_cm undefined for v_target < 0 m/s"


kto :: (RealFloat f, Floating f) => BreakPosition -> Dimensionless f
kto FreightTrainG   = 0.16 *~ one
kto FreightTrainP   = 0.20 *~ one
kto PassangerTrainP = 0.20 *~one





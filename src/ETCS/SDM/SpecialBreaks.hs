{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module ETCS.SDM.SpecialBreaks
       ( RegenerativeBreak, EddyCurrentBreak, MagneticShoeBreak, EpBreak
       , SpecialBreakType (..)
       , SpecialBreakConfiguration (..)
       , SpecialBreak, _SpecialBreak
       , sb_isServiceBreak, sb_isEmergencyBreak
       , SpecialBreakModel, _SpecialBreakModel
       , sbm_regenerativeBreak, sbm_eddyCurrentBreak
       , sbm_magneticShoeBreak, sbm_epBreak
       , ActiveBreaks
       , ab_regenerativeBreak, ab_eddyCurrentBreak
       , ab_magneticShoeBreak, ab_epBreak
       , activeEmergencyBreaks
       , activeServiceBreaks
       ) where

import           Control.Lens
import           Data.Set     (Set)
import qualified Data.Set     as Set


data RegenerativeBreak
data EddyCurrentBreak
data MagneticShoeBreak
data EpBreak

-- | the different types of 'SpecialBreak'
data SpecialBreakType t where
  RegenerativeBreak :: SpecialBreakType RegenerativeBreak
  EddyCurrentBreak :: SpecialBreakType EddyCurrentBreak
  MagneticShoeBreak :: SpecialBreakType MagneticShoeBreak
  EpBreak :: SpecialBreakType EpBreak


-- | Defines on which kind of breaking a 'SpecialBreak' is active
data SpecialBreakConfiguration =
  AffectsEmergencyBreakOnly |
  AffectsServiceBreakOnly |
  AffectsBothBreaks
  deriving (Show, Read, Eq, Ord)

-- | represents the configuration of a 'SpecialBreak'
data SpecialBreak t =
  SpecialBreak {
    _sb_type      :: SpecialBreakType t,
    _sb_interface :: Maybe SpecialBreakConfiguration
    }



-- | the model of a the special break configuration
data SpecialBreakModel =
  SpecialBreakModel {
    _sbm_regenerativeBreak :: SpecialBreak RegenerativeBreak,
    _sbm_eddyCurrentBreak  :: SpecialBreak EddyCurrentBreak,
    _sbm_magneticShoeBreak :: SpecialBreak MagneticShoeBreak,
    _sbm_epBreak           :: SpecialBreak EpBreak
    } deriving (Show, Eq)



data ActiveBreaks =
  ActiveBreaks {
    _ab_regenerativeBreak :: Bool,
    _ab_eddyCurrentBreak  :: Bool,
    _ab_magneticShoeBreak :: Bool,
    _ab_epBreak           :: Bool
    } deriving (Eq, Ord, Show)


deriving instance Eq (SpecialBreakType t)
deriving instance Ord (SpecialBreakType t)
deriving instance Eq (SpecialBreak t)
deriving instance Ord (SpecialBreak t)
deriving instance Show (SpecialBreak t)
deriving instance Show (SpecialBreakType t)

makePrisms ''SpecialBreakType
makePrisms ''SpecialBreakConfiguration
makeLenses ''SpecialBreak
makeLenses ''SpecialBreakModel
makePrisms ''SpecialBreakModel
makeLenses ''ActiveBreaks


activeEmergencyBreaks, activeServiceBreaks :: SpecialBreakModel -> ActiveBreaks
activeEmergencyBreaks = activeBreaksOn AffectsEmergencyBreakOnly
activeServiceBreaks = activeBreaksOn AffectsServiceBreakOnly


activeBreaksOn :: SpecialBreakConfiguration -> SpecialBreakModel -> ActiveBreaks
activeBreaksOn p m = ActiveBreaks
  (specialBreakAffected p sbm_regenerativeBreak m)
  (specialBreakAffected p sbm_eddyCurrentBreak m)
  (specialBreakAffected p sbm_magneticShoeBreak m)
  (specialBreakAffected p sbm_epBreak m)

specialBreakAffected ::
  SpecialBreakConfiguration -> Lens' SpecialBreakModel (SpecialBreak t) ->
  SpecialBreakModel -> Bool
specialBreakAffected p l a =
  maybe False (\c -> c == AffectsBothBreaks || c == p) $
  a ^. l . sb_interface


_SpecialBreak :: Prism' (SpecialBreakType t, Maybe SpecialBreakConfiguration)
                (SpecialBreak t)
_SpecialBreak = prism' fromSpecialBreak toSpecialBreak
  where fromSpecialBreak sb = (sb ^. sb_type, sb ^. sb_interface)
        toSpecialBreak (t, i) =
          let sb = SpecialBreak t i
          in if (validSpecialBreakConfiguration sb) then Just sb else Nothing





validSpecialBreakConfiguration :: SpecialBreak t -> Bool
validSpecialBreakConfiguration sb =
  case sb ^. sb_interface of
  Nothing -> True
  Just bc -> Set.member bc  . validBreakConfigurations $ sb ^. sb_type
    where
      validBreakConfigurations ::
        SpecialBreakType t -> Set SpecialBreakConfiguration
      validBreakConfigurations RegenerativeBreak =
        Set.fromList [ AffectsEmergencyBreakOnly, AffectsServiceBreakOnly
                     , AffectsBothBreaks ]
      validBreakConfigurations EddyCurrentBreak  =
        Set.fromList [ AffectsEmergencyBreakOnly, AffectsServiceBreakOnly
                     , AffectsBothBreaks ]
      validBreakConfigurations MagneticShoeBreak =
        Set.fromList [ AffectsEmergencyBreakOnly ]
      validBreakConfigurations EpBreak  =
        Set.fromList [ AffectsServiceBreakOnly, AffectsBothBreaks ]








sb_isServiceBreak :: SpecialBreak t -> Bool
sb_isServiceBreak b =
  let i = (b ^. sb_interface)
  in (i == Just AffectsServiceBreakOnly) || (i == Just AffectsBothBreaks)

sb_isEmergencyBreak :: SpecialBreak t -> Bool
sb_isEmergencyBreak b =
  let i = (b ^. sb_interface)
  in (i == Just AffectsEmergencyBreakOnly) || (i == Just AffectsBothBreaks)













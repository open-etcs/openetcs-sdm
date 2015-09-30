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
       , sbIsServiceBreak, sbIsEmergencyBreak
       , SpecialBreakModel, _SpecialBreakModel
       , sbmRegenerativeBreak, sbmEddyCurrentBreak
       , sbmMagneticShoeBreak, sbmEpBreak
       , ActiveBreaks
       , abRegenerativeBreak, abEddyCurrentBreak
       , abMagneticShoeBreak, abEpBreak
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
    _sbType      :: SpecialBreakType t,
    _sbInterface :: Maybe SpecialBreakConfiguration
    }



-- | the model of a the special break configuration
data SpecialBreakModel =
  SpecialBreakModel {
    _sbmRegenerativeBreak :: SpecialBreak RegenerativeBreak,
    _sbmEddyCurrentBreak  :: SpecialBreak EddyCurrentBreak,
    _sbmMagneticShoeBreak :: SpecialBreak MagneticShoeBreak,
    _sbmEpBreak           :: SpecialBreak EpBreak
    } deriving (Show, Eq)



data ActiveBreaks =
  ActiveBreaks {
    _abRegenerativeBreak :: Bool,
    _abEddyCurrentBreak  :: Bool,
    _abMagneticShoeBreak :: Bool,
    _abEpBreak           :: Bool
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
  (specialBreakAffected p sbmRegenerativeBreak m)
  (specialBreakAffected p sbmEddyCurrentBreak m)
  (specialBreakAffected p sbmMagneticShoeBreak m)
  (specialBreakAffected p sbmEpBreak m)

specialBreakAffected ::
  SpecialBreakConfiguration -> Lens' SpecialBreakModel (SpecialBreak t) ->
  SpecialBreakModel -> Bool
specialBreakAffected p l a =
  maybe False (\c -> c == AffectsBothBreaks || c == p) $
  a ^. l . sbInterface


_SpecialBreak :: Prism' (SpecialBreakType t, Maybe SpecialBreakConfiguration)
                (SpecialBreak t)
_SpecialBreak = prism' fromSpecialBreak toSpecialBreak
  where fromSpecialBreak sb = (sb ^. sbType, sb ^. sbInterface)
        toSpecialBreak (t, i) =
          let sb = SpecialBreak t i
          in if validSpecialBreakConfiguration sb then Just sb else Nothing





validSpecialBreakConfiguration :: SpecialBreak t -> Bool
validSpecialBreakConfiguration sb =
  case sb ^. sbInterface of
  Nothing -> True
  Just bc -> Set.member bc  . validBreakConfigurations $ sb ^. sbType
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








sbIsServiceBreak :: SpecialBreak t -> Bool
sbIsServiceBreak b =
  let i = (b ^. sbInterface)
  in (i == Just AffectsServiceBreakOnly) || (i == Just AffectsBothBreaks)

sbIsEmergencyBreak :: SpecialBreak t -> Bool
sbIsEmergencyBreak b =
  let i = (b ^. sbInterface)
  in (i == Just AffectsEmergencyBreakOnly) || (i == Just AffectsBothBreaks)













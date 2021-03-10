module Ability ( AbilityType(..)
               , AbilityMod(..)
               , AbilityValue(..)
               , Abilities
               , emptyAbilities
               , getAbility
               , updateAbility
               ) where

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as Map

import PrettyPrint

data AbilityType = Strength
                 | Dexterity
                 | Constitution
                 | Intelligence
                 | Wisdom
                 | Charisma
                 deriving (Eq, Ord, Read, Show, Enum)

instance PrettyPrint AbilityType where
  pp Strength     = "Str"
  pp Dexterity    = "Dex"
  pp Constitution = "Con"
  pp Intelligence = "Int"
  pp Wisdom       = "Wis"
  pp Charisma     = "Char"

newtype AbilityMod = AbilityMod { getAbilityMod :: Int }
  deriving (Eq, Ord, Read, Show)

newtype AbilityValue = AbilityValue { getAbilityValue :: Int }
  deriving (Eq, Ord, Read, Show)

newtype Abilities = Abilities { getAbilities :: Map AbilityType (AbilityValue, AbilityMod) }
  deriving (Eq, Ord, Read, Show)

instance PrettyPrint Abilities where
  pp = unlines . map toPP .  Map.toList . getAbilities where
    toPP ::  (AbilityType, (AbilityValue, AbilityMod)) -> String
    toPP (at, (av, am)) = show at ++ " " ++ (show . getAbilityMod) am ++ " (" ++ (show . getAbilityValue) av ++ ")"

calcModifier :: AbilityValue -> AbilityMod
calcModifier = AbilityMod . flip div 2 . subtract 10 . getAbilityValue

toAbilityPair :: AbilityValue -> (AbilityValue, AbilityMod)
toAbilityPair = id &&& calcModifier

getAbility :: AbilityType -> Abilities -> (AbilityType, (AbilityValue, AbilityMod))
getAbility a = ((,) a) . (Map.! a) . getAbilities

updateAbility :: AbilityType -> AbilityValue -> Abilities -> Abilities
updateAbility a av = Abilities . Map.insert a (toAbilityPair av) . getAbilities

emptyAbilities :: Abilities
emptyAbilities = Abilities . Map.fromList . map (toEnum &&& toAbilityPair . toValue) $ [0..5] where
  toValue :: Int -> AbilityValue
  toValue = AbilityValue . const 0

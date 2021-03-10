module Ability ( Ability(..)
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

data Ability = Strength
             | Dexterity
             | Constitution
             | Intelligence
             | Wisdom
             | Charisma
             deriving (Eq, Ord, Read, Show, Enum)

newtype AbilityMod = AbilityMod { getAbilityMod :: Int }
  deriving (Eq, Ord, Read, Show)

newtype AbilityValue = AbilityValue { getAbilityValue :: Int }
  deriving (Eq, Ord, Read, Show)

newtype Abilities = Abilities { getAbilities :: Map Ability (AbilityValue, AbilityMod) }
  deriving (Eq, Ord, Read, Show)

calcModifier :: AbilityValue -> AbilityMod
calcModifier = AbilityMod . flip div 2 . subtract 10 . getAbilityValue

toAbilityPair :: AbilityValue -> (AbilityValue, AbilityMod)
toAbilityPair = id &&& calcModifier

emptyAbilities :: Abilities
emptyAbilities = Abilities . Map.fromList . map (toEnum &&& toAbilityPair . toValue) $ [0..5] where
  toValue :: Int -> AbilityValue
  toValue = AbilityValue . const 0

getAbility :: Ability -> Abilities -> (AbilityValue, AbilityMod)
getAbility a = (Map.! a) . getAbilities

updateAbility :: Ability -> AbilityValue -> Abilities -> Abilities
updateAbility a av = Abilities . Map.insert a (toAbilityPair av) . getAbilities

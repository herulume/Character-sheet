module Skill ( SkillType(..)
             , Skill(..)
             , Abilities
             , emptySkills
             , getSkill
             ) where

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as Map

import Ability

data SkillType = Acrobatics
               | AnimalHandling
               | Arcana
               | Athletics
               | Deception
               | History
               | Insight
               | Intimidation
               | Medicine
               | Nature
               | Perception
               | Performance
               | Persuasion
               | Religion
               | SleightOfHand
               | Stealth
               | Survival
               deriving (Eq, Ord, Read, Show, Enum)

data Skill = Skill { abilityModifier :: AbilityType
                   , proficient :: Bool
                   , expertise :: Bool
                   } deriving (Eq, Ord, Read, Show)

newtype Skills = Skills { getSkills :: Map SkillType Skill }
  deriving (Eq, Ord, Read, Show)

getSkill :: SkillType -> Skills -> (SkillType, Skill)
getSkill s = ((,) s) . (Map.! s) . getSkills

emptySkills :: Skills
emptySkills = Skills . Map.fromList . map (toEnum &&& toSkill . toMod . toEnum) $ [0..16] where
  toSkill :: AbilityType -> Skill
  toSkill a = Skill a False False

  toMod :: SkillType -> AbilityType
  toMod Acrobatics = Dexterity
  toMod AnimalHandling = Wisdom
  toMod Arcana = Intelligence
  toMod Athletics = Strength
  toMod Deception = Charisma
  toMod History = Intelligence
  toMod Insight = Wisdom
  toMod Intimidation = Charisma
  toMod Medicine = Wisdom
  toMod Nature = Intelligence
  toMod Perception = Wisdom
  toMod Performance = Charisma
  toMod Persuasion = Charisma
  toMod Religion = Intelligence
  toMod SleightOfHand = Dexterity
  toMod Stealth = Dexterity
  toMod Survival = Wisdom

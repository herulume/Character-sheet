module Skill ( SkillType(..)
             , Skill(..)
             , Abilities
             , emptySkills
             , getSkill
             , becomeProficient
             , becomeExpert
             ) where

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as Map

import Ability
import PrettyPrint

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

instance PrettyPrint SkillType where
  pp AnimalHandling = "Animal Handling"
  pp SleightOfHand = "Sleight of Hand"
  pp s = show s

data Skill = Skill { abilityModifier :: AbilityType
                   , proficient :: Bool
                   , expertise :: Bool
                   } deriving (Eq, Ord, Read, Show)

instance PrettyPrint Skill where
  pp (Skill am _ True)      = "(" ++ pp am ++ ") *"
  pp (Skill am True False)  = "(" ++ pp am ++ ") +"
  pp (Skill am False False) = "(" ++ pp am ++ ")"

newtype Skills = Skills { getSkills :: Map SkillType Skill }
  deriving (Eq, Ord, Read, Show)

instance PrettyPrint Skills where
  pp = unlines . map toPP .  Map.toList . getSkills where
    toPP ::  (SkillType, Skill) -> String
    toPP (st, s) = pp st ++ " " ++ pp s

getSkill :: SkillType -> Skills -> (SkillType, Skill)
getSkill s = ((,) s) . (Map.! s) . getSkills

becomeExpert :: SkillType -> Skills -> Skills
becomeExpert s = Skills . Map.adjust f s . getSkills where
  f :: Skill -> Skill
  f (Skill am True _) = Skill am True True
  f s = s

becomeProficient :: SkillType -> Skills -> Skills
becomeProficient s = Skills . Map.adjust f s . getSkills where
  f :: Skill -> Skill
  f (Skill am False False) = Skill am True False
  f s = s

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

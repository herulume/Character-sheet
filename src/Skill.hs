module Skill ( SkillType(..)
             , Skill(..)
             , Skills
             , Proficiency(..)
             , Expertise(..)
             , emptySkills
             , getSkill
             , becomeProficient
             , becomeExpert
             , becomeProficientL
             , becomeExpertL
             , skillsToList
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
               | Investigation
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

data Proficiency = Proficient
                 | NotProficient
                 deriving (Eq, Ord, Read, Show)

data Expertise = Expert
               | NotExpert
                 deriving (Eq, Ord, Read, Show)

data Skill = Skill { abilityModifier :: AbilityType
                   , proficient :: Proficiency
                   , expert :: Expertise
                   } deriving (Eq, Ord, Read, Show)

instance PrettyPrint Skill where
  pp (Skill am _ Expert)                = "(" ++ pp am ++ ") *"
  pp (Skill am Proficient NotExpert)    = "(" ++ pp am ++ ") +"
  pp (Skill am NotProficient NotExpert) = "(" ++ pp am ++ ")"

newtype Skills = Skills { getSkills :: Map SkillType Skill }
  deriving (Eq, Ord, Read, Show)

instance PrettyPrint Skills where
  pp = unlines . map toPP . skillsToList  where
    toPP ::  (SkillType, Skill) -> String
    toPP (st, s) = unwords [pp st,  pp s]

skillsToList :: Skills -> [(SkillType, Skill)]
skillsToList = Map.toList . getSkills

getSkill :: SkillType -> Skills -> (SkillType, Skill)
getSkill s = ((,) s) . (Map.! s) . getSkills

becomeExpert :: SkillType -> Skills -> Skills
becomeExpert s = Skills . Map.adjust f s . getSkills where
  f :: Skill -> Skill
  f (Skill am Proficient _) = Skill am Proficient Expert
  f x = x

becomeExpertL :: [SkillType] -> Skills -> Skills
becomeExpertL = flip (foldr becomeExpert)

becomeProficient :: SkillType -> Skills -> Skills
becomeProficient s = Skills . Map.adjust f s . getSkills where
  f :: Skill -> Skill
  f (Skill am NotProficient NotExpert) = Skill am Proficient NotExpert
  f x = x

becomeProficientL :: [SkillType] -> Skills -> Skills
becomeProficientL = flip (foldr becomeProficient)

emptySkills :: Skills
emptySkills = Skills . Map.fromList . map (toEnum &&& toSkill . toMod . toEnum) $ [0..17] where
  toSkill :: AbilityType -> Skill
  toSkill a = Skill a NotProficient NotExpert

  toMod :: SkillType -> AbilityType
  toMod Acrobatics = Dexterity
  toMod AnimalHandling = Wisdom
  toMod Arcana = Intelligence
  toMod Athletics = Strength
  toMod Deception = Charisma
  toMod History = Intelligence
  toMod Insight = Wisdom
  toMod Intimidation = Charisma
  toMod Investigation = Intelligence
  toMod Medicine = Wisdom
  toMod Nature = Intelligence
  toMod Perception = Wisdom
  toMod Performance = Charisma
  toMod Persuasion = Charisma
  toMod Religion = Intelligence
  toMod SleightOfHand = Dexterity
  toMod Stealth = Dexterity
  toMod Survival = Wisdom

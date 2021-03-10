module Sheet where

import Control.Arrow
import Data.Function ((&))

import Ability
import Skill
import PrettyPrint

data Sheet = Sheet { name :: String
                   , abilities :: Abilities
                   , skills :: Skills
                   , p :: Int
                   } deriving (Eq, Ord, Read, Show)

instance PrettyPrint Sheet where
  pp (Sheet n a s _) = n ++ "\nAbilities:\n" ++ pp a ++ "\n\n Skills:\n" ++ pp s

foo :: Sheet
foo = Sheet "Elwe" as ss 3 where
  as = emptyAbilities
       & updateAbility Strength (AbilityValue 10)
       & updateAbility Dexterity (AbilityValue 20)
       & updateAbility Constitution (AbilityValue 16)
       & updateAbility Intelligence (AbilityValue 13)
       & updateAbility Wisdom (AbilityValue 13)
       & updateAbility Charisma (AbilityValue 17)
  ss = emptySkills
       & becomeProficient Acrobatics
       & becomeProficient Deception
       & becomeProficient Intimidation
       & becomeProficient Investigation
       & becomeProficient Perception
       & becomeProficient Persuasion
       & becomeProficient SleightOfHand
       & becomeProficient Stealth
       & becomeExpert Investigation
       & becomeExpert Perception
       & becomeExpert Stealth

calcSkillMod :: SkillType -> Sheet -> Int
calcSkillMod s =  f .  ((abilities &&& snd . (getSkill s) . skills) &&& p) where
  f :: ((Abilities, Skill), Int) -> Int
  f ((a, (Skill at True True)), p) = ((getAbilityMod . snd . snd . getAbility at) a) + 2 * p
  f ((a, (Skill at True False)), p) = ((getAbilityMod . snd . snd . getAbility at) a) + p
  f ((a, (Skill at False _)), _) = ((getAbilityMod . snd . snd . getAbility at) a)

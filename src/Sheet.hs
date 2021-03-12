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
  pp (Sheet n a s _) = n ++ "\nAbilities:\n" ++ pp a ++ "\nSkills:\n" ++ pp s

foo :: Sheet
foo = Sheet "Elwe" as ss 3 where
  -- need to introduce some list constructors
  as = toAbilities [10, 20, 16, 13, 13, 17]
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
calcSkillMod s =  toValue .  ((abilities &&& snd . (getSkill s) . skills) &&& p) where
  toAbilityMod :: AbilityType -> Abilities -> Int
  toAbilityMod = ((getAbilityMod . snd . snd) .) . getAbility

  toValue :: ((Abilities, Skill), Int) -> Int
  toValue ((a, (Skill at Proficient Expert)), p)    = toAbilityMod at a + 2 * p
  toValue ((a, (Skill at Proficient NotExpert)), p) = toAbilityMod at a + p
  toValue ((a, (Skill at NotProficient _)), _)      = toAbilityMod at a

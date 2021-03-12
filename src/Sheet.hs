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
calcSkillMod s =  f .  ((abilities &&& snd . (getSkill s) . skills) &&& p) where
  toAbilityMod :: AbilityType -> Abilities -> Int
  toAbilityMod = ((getAbilityMod . snd . snd) .) . getAbility

  f :: ((Abilities, Skill), Int) -> Int
  -- Boolean blindness -.-
  f ((a, (Skill at True True)), p)  = toAbilityMod at a + 2 * p
  f ((a, (Skill at True False)), p) = toAbilityMod at a + p
  f ((a, (Skill at False _)), _)    = toAbilityMod at a

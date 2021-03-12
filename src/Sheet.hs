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
  pp sh@(Sheet n a s _) = n ++ "\nAbilities:\n" ++ pp a ++ "\nSkills:\n" ++ (unlines . map (toPP sh) . skillsToList) s where
    toPP :: Sheet -> (SkillType, Skill) -> String
    toPP sh' (st, sk) = unwords [pp st, pp sk, show (calcSkillMod st sh')]

foo :: Sheet
foo = Sheet "Elwe" as ss 3 where
  as = toAbilities [10, 20, 16, 13, 13, 17]
  ss = emptySkills
       & becomeProficientL [Acrobatics, Deception, Intimidation, Investigation, Perception, Persuasion, SleightOfHand, Stealth]
       & becomeExpertL [Investigation, Perception, Stealth]

calcSkillMod :: SkillType -> Sheet -> Int
calcSkillMod s =  toValue .  ((abilities &&& snd . (getSkill s) . skills) &&& p) where
  toAbilityMod :: AbilityType -> Abilities -> Int
  toAbilityMod = ((getAbilityMod . snd . snd) .) . getAbility

  toValue :: ((Abilities, Skill), Int) -> Int
  toValue ((a, (Skill at Proficient Expert)), pv)    = toAbilityMod at a + 2 * pv
  toValue ((a, (Skill at Proficient NotExpert)), pv) = toAbilityMod at a + pv
  toValue ((a, (Skill at NotProficient _)), _)       = toAbilityMod at a

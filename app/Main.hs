module Main where

import Ability
import Skill
import PrettyPrint

main :: IO ()
main = putStrLn (pp emptyAbilities) >> (putStrLn . pp . f) emptySkills where
  f = becomeExpert Persuasion . becomeProficient Persuasion . becomeProficient Deception

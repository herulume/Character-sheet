module PrettyPrint where

class Show a => PrettyPrint a where
  pp :: a -> String

module Main where

import Sheet
import PrettyPrint

main :: IO ()
main = putStr . pp $ foo

module Main (main) where

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Hello, World!" (400, 400) (10, 10)) white (Circle 80)


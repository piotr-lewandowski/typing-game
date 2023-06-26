module Main (main) where

import Test.Hspec

import GameSpec
import HighScoreSpec

main :: IO ()
main = hspec $ do
    gameTests
    highScoreTests

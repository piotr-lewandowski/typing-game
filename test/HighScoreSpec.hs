module HighScoreSpec where

import HighScore
import Lens.Micro
import Test.Hspec
import Test.QuickCheck

instance Arbitrary HighScore where
    arbitrary = do
        name <- arbitrary
        score <- arbitrary
        return $ HighScore name score

highScoreTests :: Spec
highScoreTests =
    describe "High scores" $ do
        it "Lists should have at most 8 elements" $
            property lengthPreservation
        it "Inserting a score should not change the order of the list" $
            property sorting

isSorted :: [HighScore] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x : y : xs) = x ^. score >= y ^. score && isSorted (y : xs)

sorting :: [HighScore] -> Bool
sorting scores = isSorted $ foldl insertScore [] scores

lengthPreservation :: [HighScore] -> Bool
lengthPreservation scores = length (foldl insertScore [] scores) <= 8
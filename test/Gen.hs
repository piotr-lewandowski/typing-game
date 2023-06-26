module Gen where

import Test.QuickCheck

import Game
import Lens.Micro

nonNegativeInt :: Gen Int
nonNegativeInt = abs <$> (arbitrary :: Gen Int)

positiveInt :: Gen Int
positiveInt = (+ 1) . abs <$> (arbitrary :: Gen Int)

nonNegativeFloat :: Gen Float
nonNegativeFloat = abs <$> (arbitrary :: Gen Float)

positiveFloat :: Gen Float
positiveFloat = (+ 1) . abs <$> (arbitrary :: Gen Float)

instance Arbitrary SnippetInProgress where
    arbitrary = do
        snipType <- arbitrary
        snipLeft <- arbitrary
        snipProgress <- arbitrary
        return $ SnippetInProgress snipType snipLeft snipProgress

instance Arbitrary Snippet where
    arbitrary = do
        snipType <- arbitrary
        snipContent <- arbitrary
        return $ Snippet snipType snipContent

instance Arbitrary SnippetType where
    arbitrary = elements [Bug, Task, Story]

instance Arbitrary Game where
    arbitrary = do
        snips <- listOf1 arbitrary
        tLeft <- positiveFloat
        tInit <- positiveFloat
        score <- nonNegativeInt
        lLeft <- positiveInt
        lInit <- nonNegativeInt
        lNum <- positiveInt
        inProgress <- arbitrary
        return $ Game snips inProgress tLeft (tInit + tLeft) score lLeft (Level lNum (lInit + lLeft) snips)

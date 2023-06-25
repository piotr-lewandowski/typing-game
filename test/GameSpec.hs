module GameSpec where

import Test.Hspec
import Test.QuickCheck
import Lens.Micro
import InputEvents
import Gen
import Control.Monad (foldM)

import Game

gameTests :: Spec
gameTests = do
    describe "Game" $ do
        it "You should loose after going to 0 lifes" $
            property loosing
        it "You should win after completing all snippets" $
            property winning
        it "Completing a snippet should increase score" $
            property scoring
        it "Letters that don't match the snippet should not change the game" $
            property invalidInput
        it "Time updates should reduce the timer" $
            property updating

loosing :: Game -> Bool
loosing game = checkGameLost (game & lifesLeft .~ 0) == Left (GameLost (game^.currentScore))

winning :: Game -> Bool
winning game = setNextSnippet (game & remainingSnippets .~ []) == Left (GameWon (game^.currentScore))

scoring :: Game -> Bool
scoring game = case res of
    Left _ -> False
    Right resultGame -> resultGame^.currentScore > game^.currentScore
    where
        inputs = makeCompletingInputs game
        res = applyInputs inputs game

        applyInputs :: [InputEvents] -> Game -> Either GameResult Game
        applyInputs inputs game = foldM (flip handleGameInput) game inputs

        makeCompletingInputs :: Game -> [InputEvents]
        makeCompletingInputs game = map Typed (game^.selectedSnippet.snippetLeft) ++ [Confirm]

invalidInput :: Game -> Bool
invalidInput game = res == Right gameExpectingB
    where
        res = handleGameInput (Typed 'a') gameExpectingB
        gameExpectingB = game & selectedSnippet.snippetLeft .~ "b" & selectedSnippet.snippetProgress .~ ""

updating :: Game -> Float -> Bool
updating game dt = case res of
    Left _ -> game^.timeLeft - dt <= 0
    Right resultGame -> resultGame^.timeLeft < game^.timeLeft
    where
        res = updateGame 1 game


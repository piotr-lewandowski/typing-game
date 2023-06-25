{-# LANGUAGE TemplateHaskell #-}
module Menu where

import Config
import Game
import InputEvents
import Lens.Micro.TH
import Lens.Micro

data MenuAction = StartGame | HighScores | LevelSelect | NameChange deriving (Show)

data MenuState = MenuState { _actions :: [MenuAction], _selectedAction :: Int } deriving (Show)

makeLenses ''MenuState

handleMenuInput :: InputEvents -> MenuState -> Either MenuAction MenuState
handleMenuInput GoUp menu = Right $ menu & selectedAction -~ 1 & clampSelectedAction
handleMenuInput GoDown menu = Right $ menu & selectedAction +~ 1 & clampSelectedAction
handleMenuInput Confirm menu = Left $ (menu^.actions) !! (menu^.selectedAction)
handleMenuInput _ menu = Right menu

clampSelectedAction :: MenuState -> MenuState
clampSelectedAction menu = menu & selectedAction .~ (menu^.selectedAction) `mod` len
    where 
        len = length $ menu^.actions

mainMenu :: MenuState
mainMenu = MenuState [StartGame, HighScores, LevelSelect, NameChange] 0
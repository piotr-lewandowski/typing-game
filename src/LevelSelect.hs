{-# LANGUAGE TemplateHaskell #-}
module LevelSelect where

import InputEvents
import Lens.Micro.TH
import Lens.Micro
import Game

data LevelSelectState = LevelSelectState { _allLevels :: [Level], _selectedLevel :: Int } deriving (Show)

makeLenses ''LevelSelectState

handleLevelSelectInput :: InputEvents -> LevelSelectState -> Either Level LevelSelectState
handleLevelSelectInput GoUp levelSelect = Right $ levelSelect & selectedLevel -~ 1 & clampSelectedLevel
handleLevelSelectInput GoDown levelSelect = Right $ levelSelect & selectedLevel +~ 1 & clampSelectedLevel
handleLevelSelectInput Confirm levelSelect = Left $ (levelSelect^.allLevels) !! (levelSelect^.selectedLevel)
handleLevelSelectInput _ levelSelect = Right levelSelect

clampSelectedLevel :: LevelSelectState -> LevelSelectState
clampSelectedLevel levelSelect = levelSelect & selectedLevel .~ (levelSelect^.selectedLevel) `mod` len
    where 
        len = length $ levelSelect^.allLevels
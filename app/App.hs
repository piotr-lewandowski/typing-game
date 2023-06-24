{-# LANGUAGE TemplateHaskell #-}
module App where

import Game
import Config
import Menu
import Input
import Control.Monad.State.Strict
import Lens.Micro.TH
import Lens.Micro

data Stage = Playing Game | Menu MenuState | Lost Score | Won Score

data App = App
    { _currentStage :: Stage
    , _activeConfig :: Config
    , _activeLevel :: Level }

makeLenses ''App

type AppState = State App

updateApp :: Float -> AppState ()
updateApp dt = do
    app <- get
    case app^.currentStage of
        Playing game -> case updateGame dt game of
            Left (GameLost score) -> put $ app & currentStage .~ Lost score
            Left (GameWon score) -> put $ app & currentStage .~ Won score
            Right game' -> put $ app & currentStage .~ Playing game'
        _ -> return ()
    
handleAppInput :: InputEvents -> AppState ()
handleAppInput (Resize (w, h)) = do
    app <- get
    put $ app & activeConfig.width .~ w & activeConfig.height .~ h
handleAppInput event = do
    app <- get
    case app^.currentStage of
        Playing game -> case handleGameInput event game of
            Left (GameLost score) -> put $ app & currentStage .~ Lost score
            Left (GameWon score) -> put $ app & currentStage .~ Won score
            Right game' -> put $ app & currentStage .~ Playing game'
        Menu menu -> case handleMenuInput event menu of
            Left StartGame -> put $ app & currentStage .~ Playing (newGame $ app^.activeLevel)
            Left HighScores -> put $ app & currentStage .~ Playing (newGame $ app^.activeLevel)
            Right menu' -> put $ app & currentStage .~ Menu menu'
        _ -> put $ app & currentStage .~ Menu mainMenu

handleMaybeAppInput :: Maybe InputEvents -> AppState ()
handleMaybeAppInput Nothing = return ()
handleMaybeAppInput (Just event) = handleAppInput event
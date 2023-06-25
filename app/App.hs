{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
module App where

import Game
import Config
import Menu
import Input
import Lens.Micro.TH
import Lens.Micro
import Images
import FRP.Yampa

data Stage = Playing Game | Menu MenuState | Lost Score | Won Score

data App = App
    { _currentStage :: Stage
    , _activeConfig :: Config
    , _images :: Images
    , _activeLevel :: Level
    }

makeLenses ''App

updateApp :: Float -> App -> App
updateApp dt app =
    case app^.currentStage of
        Playing game -> case updateGame dt game of
            Left (GameLost s) -> app & currentStage .~ Lost s
            Left (GameWon s) -> app & currentStage .~ Won s
            Right game' -> app & currentStage .~ Playing game'
        _ -> app
    
handleAppInput :: InputEvents -> App -> App
handleAppInput (Resize (w, h)) app = app & activeConfig.width .~ w & activeConfig.height .~ h
handleAppInput event app =
    case app^.currentStage of
        Playing game -> case handleGameInput event game of
            Left (GameLost s) -> app & currentStage .~ Lost s
            Left (GameWon s) -> app & currentStage .~ Won s
            Right game' -> app & currentStage .~ Playing game'
        Menu menu -> case handleMenuInput event menu of
            Left StartGame -> app & currentStage .~ Playing (newGame $ app^.activeLevel)
            Left HighScores -> app
            Left LevelSelect -> app
            Right menu' -> app & currentStage .~ Menu menu'
        _ -> app & currentStage .~ Menu mainMenu

handleMaybeAppInput :: Event InputEvents -> App -> App
handleMaybeAppInput NoEvent = id
handleMaybeAppInput (Event e) = handleAppInput e

timeDifference :: SF () Float
timeDifference = iterFrom (\_ _ dt _ -> dt) 0 >>> arr realToFrac

inputSF :: App -> SF (Event InputEvents) App
inputSF app = loopPre app $ proc (e, app') -> do
    app'' <- arr (uncurry handleMaybeAppInput) -< (e, app')
    dt <- timeDifference -< ()
    app''' <- arr (uncurry updateApp) -< (realToFrac dt, app'')
    returnA -< (app''', app''')
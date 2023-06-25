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
import HighScore
import NameChange
import System.IO.Unsafe
import LevelSelect

data Stage 
    = Playing Game 
    | Menu MenuState 
    | Lost Score 
    | Won Score
    | ViewingScores [HighScore]
    | ChangingName NameChangeState
    | ChoosingLevel LevelSelectState

data App = App
    { _currentStage :: Stage
    , _activeConfig :: Config
    , _images :: Images
    , _activeLevel :: Level
    , _highScores :: [HighScore]
    }

makeLenses ''App

updateApp :: Float -> App -> App
updateApp dt ap =
    case ap^.currentStage of
        Playing game -> case updateGame dt game of
            Left (GameLost s) -> ap & currentStage .~ Lost s
            Left (GameWon s) -> ap & currentStage .~ Won s
            Right game' -> ap & currentStage .~ Playing game'
        _ -> ap
    
handleAppInput :: InputEvents -> App -> App
handleAppInput (Resize (w, h)) ap = ap & activeConfig.width .~ w & activeConfig.height .~ h
handleAppInput ev ap =
    case ap^.currentStage of
        Playing game -> case handleGameInput ev game of
            Left (GameLost s) -> ap & currentStage .~ Lost s
            Left (GameWon s) -> ap & currentStage .~ Won s & updateScores s
            Right game' -> ap & currentStage .~ Playing game'
        Menu menu -> case handleMenuInput ev menu of
            Left StartGame -> ap & currentStage .~ Playing (newGame $ ap^.activeLevel)
            Left HighScores -> ap & currentStage .~ ViewingScores (ap^.highScores)
            Left LevelSelect -> ap & currentStage .~ ChoosingLevel (LevelSelectState (ap^.activeConfig.levels) 0)
            Left NameChange -> ap & currentStage .~ ChangingName (NameChangeState (ap^.activeConfig.name) "")
            Right menu' -> ap & currentStage .~ Menu menu'
        ChangingName nameChange -> case handleNameChangeInput ev nameChange of
            Left new -> ap & currentStage .~ (Menu mainMenu) & updateName new
            Right newState -> ap & currentStage .~ ChangingName newState
        ChoosingLevel levelSelect -> case handleLevelSelectInput ev levelSelect of
            Left newLevel -> ap & currentStage .~ Playing (newGame newLevel) & activeLevel .~ newLevel
            Right newState -> ap & currentStage .~ ChoosingLevel newState
        _ -> ap & currentStage .~ Menu mainMenu

handleMaybeAppInput :: Event InputEvents -> App -> App
handleMaybeAppInput NoEvent = id
handleMaybeAppInput (Event e) = handleAppInput e

timeDifference :: SF () Float
timeDifference = iterFrom (\_ _ dt _ -> dt) 0 >>> arr realToFrac

inputSF :: App -> SF (Event InputEvents) App
inputSF initialApp = loopPre initialApp $ proc (e, app') -> do
    app'' <- arr (uncurry handleMaybeAppInput) -< (e, app')
    dt <- timeDifference -< ()
    app''' <- arr (uncurry updateApp) -< (dt, app'')
    returnA -< (app''', app''')

updateScores :: Score -> App -> App
updateScores s a = seq (unsafePerformIO $ writeHighScores newScores) (a & highScores .~ newScores)
    where
        newScores = insertScore (a^.highScores) (HighScore (a^.(activeConfig.name)) s)

updateName :: Name -> App -> App
updateName n a = seq (unsafePerformIO $ writeConfig (newApp^.activeConfig)) newApp
    where 
        newApp = (a & activeConfig.name .~ n)
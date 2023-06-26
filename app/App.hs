{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module App (Stage (..), App (..), currentStage, activeConfig, images, activeLevel, highScores, appSF) where

import Config
import FRP.Yampa
import Game
import HighScore
import Images
import Input
import Lens.Micro
import Lens.Micro.TH
import LevelSelect
import Menu
import NameChange
import System.IO.Unsafe

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

appSF :: App -> SF (Event InputEvents) App
appSF initialApp = loopPre initialApp $ proc (input, previousApp) -> do
    appAfterInput <- arr (uncurry handleEventAppInput) -< (input, previousApp)
    dt <- timeDifference -< ()
    updatingApp <- arr (uncurry updateApp) -< (dt, appAfterInput)
    returnA -< (updatingApp, updatingApp)

timeDifference :: SF () Float
timeDifference = iterFrom (\_ _ dt _ -> dt) 0 >>> arr realToFrac

updateApp :: Float -> App -> App
updateApp dt ap =
    case ap ^. currentStage of
        Playing game -> case updateGame dt game of
            Left (GameLost s) -> ap & currentStage .~ Lost s
            Left (GameWon s) -> ap & currentStage .~ Won s
            Right game' -> ap & currentStage .~ Playing game'
        _ -> ap

handleAppInput :: InputEvents -> App -> App
handleAppInput (Resize newSize) ap = updateSize newSize ap
handleAppInput ev ap =
    ap & case ap ^. currentStage of
        Playing game -> case handleGameInput ev game of
            Left (GameLost s) -> currentStage .~ Lost s
            Left (GameWon s) -> (currentStage .~ Won s) . updateScores s
            Right game' -> currentStage .~ Playing game'
        Menu menu -> case handleMenuInput ev menu of
            Left StartGame -> currentStage .~ Playing (newGame $ ap ^. activeLevel)
            Left HighScores -> currentStage .~ ViewingScores (ap ^. highScores)
            Left LevelSelect -> currentStage .~ ChoosingLevel (LevelSelectState (ap ^. activeConfig . levels) 0)
            Left NameChange -> currentStage .~ ChangingName (NameChangeState (ap ^. activeConfig . name) "")
            Right menu' -> currentStage .~ Menu menu'
        ChangingName nameChange -> case handleNameChangeInput ev nameChange of
            Left new -> (currentStage .~ (Menu mainMenu)) . updateName new
            Right newState -> currentStage .~ ChangingName newState
        ChoosingLevel levelSelect -> case handleLevelSelectInput ev levelSelect of
            Left newLevel -> (currentStage .~ Playing (newGame newLevel)) . (activeLevel .~ newLevel)
            Right newState -> currentStage .~ ChoosingLevel newState
        _ -> currentStage .~ Menu mainMenu

handleEventAppInput :: Event InputEvents -> App -> App
handleEventAppInput NoEvent = id
handleEventAppInput (Event e) = handleAppInput e

updateScores :: Score -> App -> App
updateScores s a = seq (unsafePerformIO $ writeHighScores newScores) (a & highScores .~ newScores)
  where
    newScores = insertScore (a ^. highScores) (HighScore (a ^. (activeConfig . name)) s)

updateName :: Name -> App -> App
updateName n a = seq (unsafePerformIO $ writeConfig (newApp ^. activeConfig)) newApp
  where
    newApp = (a & activeConfig . name .~ n)

updateSize :: (Int, Int) -> App -> App
updateSize (w, h) a = seq (unsafePerformIO $ writeConfig (newApp ^. activeConfig)) newApp
  where
    newApp = (a & activeConfig . width .~ w & activeConfig . height .~ h)
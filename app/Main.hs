module Main (main) where

import App
import Config
import FRP.Yampa
import Graphics
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.Yampa
import HighScore
import Images
import Input
import Lens.Micro
import Menu

main :: IO ()
main = do
    config <- readConfig
    imgs <- loadImages
    scores <- readHighScores
    let w = config ^. width
        h = config ^. height
        initialState =
            App
                { _currentStage = Menu mainMenu
                , _activeConfig = config
                , _images = imgs
                , _activeLevel = head $ config ^. levels
                , _highScores = scores
                }
    playYampa
        (InWindow "Dev Sim" (w, h) (0, 0))
        white
        120
        (arr (>>= mapGlossEvents) >>> appSF initialState >>> arr renderApp)

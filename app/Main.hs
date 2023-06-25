module Main (main) where

import Graphics.Gloss
import Graphics
import Game
import Config
import Input
import Images
import App
import Lens.Micro
import FRP.Yampa
import Graphics.Gloss.Interface.FRP.Yampa
import HighScore
import Menu

main :: IO ()
main = do
    config <- readConfig
    imgs <- loadImages
    scores <- readHighScores
    let w = config^.width
        h = config^.height
        initialState = App {
            _currentStage = Menu mainMenu,
            _activeConfig = config,
            _images = imgs,
            _activeLevel = head $ config^.levels,
            _highScores = scores
        }
    playYampa
        (InWindow "Dev Sim" (w, h) (0, 0))
        white
        120
        (arr (>>= mapGlossEvents) >>> inputSF initialState >>> arr renderApp)

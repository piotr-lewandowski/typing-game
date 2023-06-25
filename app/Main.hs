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

main :: IO ()
main = do
    config <- readConfig
    imgs <- loadImages
    let w = config^.width
        h = config^.height
    playYampa
        (InWindow "Dev Sim" (w, h) (0, 0))
        white
        120
        (arr (>>= mapGlossEvents) >>> inputSF (initialState config imgs) >>> arr renderApp)


initialState :: Config -> Images -> App
initialState config imgs = App {
    _currentStage = Playing (newGame (head $ config^.levels)),
    _activeConfig = config,
    _images = imgs,
    _activeLevel = head $ config^.levels
}
module Main (main) where

import Graphics.Gloss
import Graphics
import Game
import Config
import Input
import Images
import App
import Lens.Micro
import Control.Monad.State.Strict

main :: IO ()
main = do
    config <- readConfig
    imgs <- loadImages
    let w = config^.width
        h = config^.height
    play
        (InWindow "Developer Simulator" (w, h) (0, 0))
        white
        120
        (initialState config imgs)
        renderApp
        ((execState . handleMaybeAppInput) . mapGlossEvents)
        (execState . updateApp)

initialState :: Config -> Images -> App
initialState config imgs = App {
    _currentStage = Playing (newGame (head $ config^.levels)),
    _activeConfig = config,
    _images = imgs,
    _activeLevel = head $ config^.levels
}
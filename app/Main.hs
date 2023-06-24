module Main (main) where

import Graphics.Gloss
import Graphics
import Game
import Config
import Input
import App
import Lens.Micro
import Control.Monad.State.Strict

main :: IO ()
main = do
    config <- readConfig
    let w = fromIntegral (config^.width)
        h = fromIntegral (config^.height)
    play 
        (InWindow "Developer Simulator" (w, h) (0, 0)) 
        white 
        120 
        (initialState config)
        renderApp
        ((execState . handleMaybeAppInput) . mapGlossEvents)
        (execState . updateApp)

initialState :: Config -> App
initialState config = App {
    _currentStage = Playing (newGame (head $ config^.levels)),
    _activeConfig = config,
    _activeLevel = head $ config^.levels
}
module Graphics where

import Graphics.Gloss
import Game
import Config
import App
import Menu
import Lens.Micro

renderApp :: App -> Picture
renderApp app = case app^.currentStage of
    Playing game -> renderGame (app^.activeConfig) game
    Menu menu -> renderMenu menu
    Lost score -> renderLost score
    Won score -> renderWon score

deskColor :: Color
deskColor = makeColorI 222 184 135 255

renderDesk :: Config -> Picture
renderDesk config = translate 0 movY $ color deskColor $ rectangleSolid w h
    where
        w = (fromIntegral (config^.width))
        h = 0.2 * fromIntegral (config^.height)
        movY = - fromIntegral (config^.height) / 2

renderMonitor :: Config -> Picture
renderMonitor config = color black $ rectangleSolid w h
    where
        w = 0.8 * fromIntegral (config^.width)
        h = 0.8 * fromIntegral (config^.height)

renderSnippet :: Config -> SnippetInProgress -> Picture
renderSnippet config (SnippetInProgress typ left progress) = pictures 
    [ renderSnippetBackground config
    , color c (scale 0.2 0.2 $ text progress)
    , translate (fromIntegral (length progress) * 12.0) 0 $ (scale 0.2 0.2 $ text left)
    ]
    where
        c = case typ of
            Bug -> red
            Task -> blue
            Story -> green

renderSnippetBackground :: Config -> Picture
renderSnippetBackground config = color white $ rectangleSolid w h
    where
        w = 0.7 * fromIntegral (config^.width)
        h = 0.4 * fromIntegral (config^.height)

renderGame :: Config -> Game -> Picture
renderGame config game = pictures 
    [ renderDesk config
    , renderMonitor config
    , renderSnippet config (game^.selectedSnippet)
    , renderScore config game
    , renderlifes config game
    , renderTimeLeft config game
    ]

renderScore :: Config -> Game -> Picture
renderScore config game = translate (- fromIntegral (config^.width) / 2) (fromIntegral (config^.height) / 2 - 20) $ scale 0.2 0.2 $ text $ show $ game^.score

renderMenu :: MenuState -> Picture
renderMenu menu = pictures $ zipWith renderMenuItem [0..] (menu^.actions)
    where
        renderMenuItem :: Int -> MenuAction -> Picture
        renderMenuItem i action = translate 0 (fromIntegral i * 100) $ color black $ scale 0.2 0.2 $ text $ show action

renderLost :: Score -> Picture
renderLost score = color red $ scale 0.2 0.2 $ text $ "You lost with score " ++ show score

renderWon :: Score -> Picture
renderWon score = color green $ scale 0.2 0.2 $ text $ "You won with score " ++ show score

renderlifes :: Config -> Game -> Picture
renderlifes config game = translate (fromIntegral (config^.width) / 2 - 300) (fromIntegral (config^.height) / 2 - 23) $ scale 0.2 0.2 $ text $ "Lifes left: " ++ show (game^.lifesLeft)

renderTimeLeft :: Config -> Game -> Picture
renderTimeLeft config game = translate (-200) (-50) $ scale 0.2 0.2 $ text $ "Time left: " ++ show (game^.timeLeft)
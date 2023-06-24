module Graphics where

import Graphics.Gloss
import Game
import Config
import App
import Menu
import Lens.Micro
import Images

renderApp :: App -> Picture
renderApp app = case app^.currentStage of
    Playing game -> renderGame (app^.activeConfig) (app^.images) game
    Menu menu -> renderMenu menu
    Lost s -> renderLost s
    Won s -> renderWon s

deskColor :: Color
deskColor = makeColorI 222 184 135 255

renderDesk :: Config -> Picture -> Picture
renderDesk config keyb = translate 0 movY $ color deskColor $ rectangleSolid w h
    where
        w = fromIntegral (config^.width)
        h = 0.2 * fromIntegral (config^.height)
        movY = -fromIntegral (config^.height) / 2

renderMonitor :: Config -> Picture -> Picture
renderMonitor config monit = color black $ rectangleSolid w h
    where
        w = 0.8 * fromIntegral (config^.width)
        h = 0.8 * fromIntegral (config^.height)

renderSnippet :: Config -> SnippetInProgress -> Picture
renderSnippet config (SnippetInProgress typ left progress) = pictures
    [ renderSnippetBackground config
    , color c (scale 0.2 0.2 $ text progress)
    , translate (fromIntegral (length progress) * 12.0) 0 $ scale 0.2 0.2 (text left)
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

renderGame :: Config -> Images -> Game -> Picture
renderGame config imgs game = pictures
    [ renderDesk config (imgs^.keyboardImg)
    , renderMonitor config (imgs^.monitorImg)
    , renderSnippet config (game^.selectedSnippet)
    , renderScore config game
    , renderLifes config game (imgs^.heartImg)
    , renderTimeLeft config game
    ]

renderScore :: Config -> Game -> Picture
renderScore config game = translate (- fromIntegral (config^.width) / 2) (fromIntegral (config^.height) / 2 - 40)
    $ color green
    $ scale 0.3 0.3
    $ text
    $ "$" ++ show (game^.score)

renderMenu :: MenuState -> Picture
renderMenu menu = pictures $ zipWith renderMenuItem [0..] (menu^.actions)
    where
        renderMenuItem :: Int -> MenuAction -> Picture
        renderMenuItem i action = translate 0 (fromIntegral i * 100) $ color (chooseColor i) $ scale 0.2 0.2 $ text $ show action
        chooseColor :: Int -> Color
        chooseColor i = if i == menu^.selectedAction then red else black

renderLost :: Score -> Picture
renderLost s = color red $ scale 0.2 0.2 $ text $ "You lost with score " ++ show s

renderWon :: Score -> Picture
renderWon s = color green $ scale 0.2 0.2 $ text $ "You won with score " ++ show s

renderLifes :: Config -> Game -> Picture -> Picture
renderLifes config game lifePicture = translate (fromIntegral (config^.width) / 2 - 2 * totalW) (fromIntegral (config^.height) / 2 - w / 2 - 5) $ pictures
    [ color (greyN 0.3) $ pictures $ moveList $ replicate initial lifePicture
    , color red $ pictures $ moveList $ replicate left lifePicture
    ]
    where
        moveList :: [Picture] -> [Picture]
        moveList = zipWith (`translate` 0) [0, w+5..]
        w :: Float
        w = 40.0
        totalW = w * fromIntegral initial
        h = fromIntegral (config^.height)
        left = game^.lifesLeft
        initial = game^.level^.levelLifes

renderTimeLeft :: Config -> Game -> Picture
renderTimeLeft config game = translate (fromIntegral (config^.width) / 2 - 15) 0
    $ color red
    $ rectangleSolid 30 timeLeftHeight
    where
        h = fromIntegral (config^.height)
        left = game^.timeLeft
        initial = game^.timeInitial
        timeLeftRatio = left / initial
        timeLeftHeight = h * timeLeftRatio
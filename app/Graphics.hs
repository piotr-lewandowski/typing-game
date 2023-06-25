module Graphics where

import Graphics.Gloss
import Game
import Config
import App
import Menu
import Lens.Micro
import Images
import HighScore

renderApp :: App -> Picture
renderApp app = case app^.currentStage of
    Playing game -> renderGame (app^.activeConfig) (app^.images) game
    Menu menu -> renderMenu (app^.activeConfig) menu (app^.images.titleImg)
    Lost s -> renderLost s
    Won s -> renderWon s
    ViewingScores scores -> renderHighScores config scores (app^.images.scoresImg)
    ChangingName name -> renderNameChange config name (app^.images.nameImg)
    ChoosingLevel levels -> renderLevelSelect config levels (app^.images.levelImg)
    where
        config = app^.activeConfig

deskColor :: Color
deskColor = makeColorI 222 184 135 255

renderDesk :: Config -> Picture -> Picture
renderDesk config keyb = pictures
    [ translate 0 (-movY) $ color deskColor $ rectangleSolid w h
    , translate 0 (-movY) $ scale 0.5 0.5 keyb
    ]
    where
        w = fromIntegral (config^.width)
        h = 0.2 * fromIntegral (config^.height)
        movY = fromIntegral (config^.height) / 2

renderMonitor :: Config -> Picture -> Picture
renderMonitor config monit = translate 0 (-10) $ scale 0.8 0.8 monit

renderSnippet :: Config -> SnippetInProgress -> Picture -> Picture
renderSnippet config (SnippetInProgress typ left progress) ok = pictures
    [ translate (-260) 0 $ color c (scale 0.2 0.2 $ text progress)
    , if null left then done else todo
    , renderSnippetType config typ
    ]
    where
        todo = translate (-200) 100 $ scale 0.2 0.2 $ text $ "TODO: " ++ left
        done = translate 200 0 $ scale 0.2 0.2 ok
        c = case typ of
            Bug -> red
            Task -> blue
            Story -> green

renderSnippetType:: Config -> SnippetType -> Picture
renderSnippetType config typ = pictures
    [ translate (-220) 175 $ color col $ rectangleSolid 100 50
    , translate (-260) 165 $ color white (scale 0.2 0.2 $ text txt)
    ]
    where
        col = case typ of
            Bug -> red
            Task -> blue
            Story -> green
        txt = show typ

renderGame :: Config -> Images -> Game -> Picture
renderGame config imgs game = pictures
    [ renderDesk config (imgs^.keyboardImg)
    , renderMonitor config (imgs^.monitorImg)
    , renderSnippet config (game^.selectedSnippet) (imgs^.okImg)
    , renderScore config game
    , renderLifes config game imgs
    , renderTimeLeft config game
    ]

renderScore :: Config -> Game -> Picture
renderScore config game = translate (- fromIntegral (config^.width) / 2) (fromIntegral (config^.height) / 2 - 40)
    $ color green
    $ scale 0.3 0.3
    $ text
    $ "$" ++ show (game^.currentScore)

renderMenu :: Config -> MenuState -> Picture -> Picture
renderMenu config menu title = pictures $ placedTitle : zipWith renderMenuItem [0..] (menu^.actions)
    where
        placedTitle = translate 0 (fromIntegral (config^.height)/2 - 100) title
        renderMenuItem :: Int -> MenuAction -> Picture
        renderMenuItem i action = translate ( (50 - fromIntegral (config^.width) / 2)) (100 + fromIntegral (-i) * 100) $ color (chooseColor i) $ scale 0.2 0.2 $ text $ show action
        chooseColor :: Int -> Color
        chooseColor i = if i == menu^.selectedAction then red else black

renderLost :: Score -> Picture
renderLost s = color red $ scale 0.2 0.2 $ text $ "You lost with score " ++ show s

renderWon :: Score -> Picture
renderWon s = color green $ scale 0.2 0.2 $ text $ "You won with score " ++ show s

renderLifes :: Config -> Game -> Images -> Picture
renderLifes config game imgs = translate (fromIntegral (config^.width) / 2 - 2 * totalW) (fromIntegral (config^.height) / 2 - w / 2 - 5) $ pictures
    [ pictures $ moveList $ replicate initial (imgs^.brokenHeartImg)
    , pictures $ moveList $ replicate left (imgs^.heartImg)
    ]
    where
        moveList :: [Picture] -> [Picture]
        moveList = zipWith (`translate` 0) [0, w+5..]
        w :: Float
        w = 40.0
        totalW = w * fromIntegral initial
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

renderHighScores :: Config -> [HighScore] -> Picture -> Picture
renderHighScores config scores title = pictures $ placedTitle : zipWith renderHs [0..] scores
    where
        placedTitle = translate 0 (fromIntegral (config^.height)/2 - 100) title
        renderHs :: Int -> HighScore -> Picture
        renderHs i (HighScore name s) = translate 0 (fromIntegral (-i) * 100) $ scale 0.2 0.2 $ text $ name ++ " " ++ show s

renderNameChange :: Config -> String -> Picture -> Picture
renderNameChange config name title = pictures $ placedTitle : [renderName]
    where
        placedTitle = translate 0 (fromIntegral (config^.height)/2 - 100) title
        renderName = translate 0 (fromIntegral (-1) * 100) $ scale 0.2 0.2 $ text $ name

renderLevelSelect :: Config -> [Level] -> Picture -> Picture
renderLevelSelect config levels title = pictures $ placedTitle : zipWith renderLevel [0..] levels
    where
        placedTitle = translate 0 (fromIntegral (config^.height)/2 - 100) title
        renderLevel :: Int -> Level -> Picture
        renderLevel i (Level name _ _) = translate 0 (fromIntegral (-i) * 100) $ scale 0.2 0.2 $ text $ show name
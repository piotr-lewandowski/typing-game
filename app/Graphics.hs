module Graphics(renderApp) where

import Graphics.Gloss
import Game
import Config
import App
import Menu
import Lens.Micro
import Images
import HighScore
import NameChange

renderApp :: App -> Picture
renderApp app = case app^.currentStage of
    Playing game -> renderGame (app^.activeConfig) (app^.images) game
    Menu menu -> renderMenu (app^.activeConfig) menu (app^.images.titleImg)
    Lost s -> renderLost s
    Won s -> renderWon s
    ViewingScores scores -> renderHighScores config scores (app^.images.scoresImg)
    ChangingName names -> renderNameChange config names (app^.images.nameImg)
    ChoosingLevel lvls -> renderLevelSelect config lvls (app^.images.levelImg)
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

renderMonitor :: Picture -> Picture
renderMonitor monit = translate 0 (-10) $ scale 0.8 0.8 monit

renderSnippet :: SnippetInProgress -> Picture -> Picture
renderSnippet (SnippetInProgress typ left progress) ok = pictures
    [ translate (-260) 0 $ color c (scale 0.2 0.2 $ text progress)
    , if null left then done else todo
    , renderSnippetType typ
    ]
    where
        todo = translate (-200) 100 $ scale 0.2 0.2 $ text $ "TODO: " ++ left
        done = translate 200 0 $ scale 0.2 0.2 ok
        c = case typ of
            Bug -> red
            Task -> blue
            Story -> green

renderSnippetType:: SnippetType -> Picture
renderSnippetType typ = pictures
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
    , renderMonitor (imgs^.monitorImg)
    , renderSnippet (game^.selectedSnippet) (imgs^.okImg)
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

renderMenu :: Config -> MenuState -> Picture -> Picture
renderMenu config menu title = pictures $ placedTitle : zipWith renderMenuItem [0..] (menu^.actions)
    where
        placedTitle = translate 0 (fromIntegral (config^.height)/2 - 100) title
        renderMenuItem :: Int -> MenuAction -> Picture
        renderMenuItem i action = translate (50 - fromIntegral (config^.width) / 2) (100 + fromIntegral (-i) * 100) $ color (chooseColor i) $ scale 0.2 0.2 $ text $ show action
        chooseColor :: Int -> Color
        chooseColor i = if i == menu^.selectedAction then red else black

renderLost :: Score -> Picture
renderLost s = color red $ scale 0.2 0.2 $ text $ "You lost with score " ++ show s

renderWon :: Score -> Picture
renderWon s = color green $ scale 0.2 0.2 $ text $ "You won with score " ++ show s


renderHighScores :: Config -> [HighScore] -> Picture -> Picture
renderHighScores config scores title = pictures $ placedTitle : zipWith renderHs [0..] scores
    where
        placedTitle = translate 0 (fromIntegral (config^.height)/2 - 100) title
        renderHs :: Int -> HighScore -> Picture
        renderHs i (HighScore n s) = translate (50 - fromIntegral (config^.width) / 2) (100 + fromIntegral (-i) * 50) $ scale 0.2 0.2 $ text $ n ++ " " ++ show s

renderNameChange :: Config -> NameChangeState -> Picture -> Picture
renderNameChange config (NameChangeState old new) title = pictures $ [placedTitle, renderOld, renderNew]
    where
        placedTitle = translate 0 (fromIntegral (config^.height)/2 - 100) title
        renderOld = translate (-200) (-100) $ scale 0.2 0.2 $ text $ "Current name: " ++ old
        renderNew = translate (-200) (-200) $ scale 0.2 0.2 $ text $ "New name: " ++ new

renderLevelSelect :: Config -> [Level] -> Picture -> Picture
renderLevelSelect config lvls title = pictures $ placedTitle : zipWith renderLevel [0..] lvls
    where
        placedTitle = translate 0 (fromIntegral (config^.height)/2 - 100) title
        renderLevel :: Int -> Level -> Picture
        renderLevel i (Level num _ _) = translate 0 (100 + fromIntegral (-i) * 50) $ scale 0.2 0.2 $ text $ show num
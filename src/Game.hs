{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Game where

import InputEvents
import Lens.Micro.TH
import Lens.Micro
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

type Score = Int

data SnippetType = Bug | Task | Story deriving (Generic, Show)

data Snippet = Snippet { _snippetType :: SnippetType, _snippetContent :: String } deriving (Generic, Show)

data SnippetInProgress = SnippetInProgress { _snippetInProgressType :: SnippetType, _snippetLeft :: String, _snippetProgress :: String }

data Game = Game
    { _remainingSnippets :: [Snippet]
    , _selectedSnippet :: SnippetInProgress
    , _timeLeft :: Float
    , _timeInitial :: Float
    , _currentScore :: Score
    , _lifesLeft :: Int
    , _level :: Level }

data Level = Level
    { _levelNum :: Int
    , _levelLifes :: Int
    , _levelSnippets :: [Snippet] } deriving (Generic, Show)

makeLenses ''Game
makeLenses ''Level
makeLenses ''Snippet
makeLenses ''SnippetInProgress

instance ToJSON SnippetType
instance ToJSON Snippet
instance ToJSON Level
instance FromJSON SnippetType
instance FromJSON Snippet
instance FromJSON Level

handleGameInput :: InputEvents -> Game -> Either GameResult Game
handleGameInput (Typed c) game = if c == head (game^.selectedSnippet.snippetLeft)
    then Right $ game & selectedSnippet.snippetLeft %~ tail & selectedSnippet.snippetProgress %~ (++ [c])
    else Right game
handleGameInput Confirm game = if null (game^.selectedSnippet.snippetLeft)
    then game & currentScore +~ calculateScore game & setNextSnippet
    else Right game
handleGameInput _ game = Right game

calculateScore :: Game -> Score
calculateScore game = length (game^.selectedSnippet.snippetProgress) * ceiling (game^.timeLeft)

updateGame :: Float -> Game -> Either GameResult Game
updateGame dt game = if game^.timeLeft <= 0
    then game & lifesLeft -~ 1 & checkGameLost >>= setNextSnippet
    else Right $ game & timeLeft -~ dt

checkGameLost :: Game -> Either GameResult Game
checkGameLost game = if game^.lifesLeft <= 0
    then Left $ GameLost (game^.currentScore)
    else Right game

setNextSnippet :: Game -> Either GameResult Game
setNextSnippet game = case game^.remainingSnippets of
    [] -> Left $ GameWon (game^.currentScore)
    (x:xs) -> Right $ game 
        & remainingSnippets .~ xs 
        & selectedSnippet .~ freshSnippet x 
        & timeLeft .~ calculateSnippetTime x
        & timeInitial .~ calculateSnippetTime x

freshSnippet :: Snippet -> SnippetInProgress
freshSnippet (Snippet t c) = SnippetInProgress t c ""

data GameResult = GameWon Score | GameLost Score

calculateSnippetTime :: Snippet -> Float
calculateSnippetTime (Snippet t c) = fromIntegral (length c) / 10 + case t of
    Bug -> 3
    Task -> 6
    Story -> 9

newGame :: Level -> Game
newGame l = Game
    { _remainingSnippets = tail $ l^.levelSnippets
    , _selectedSnippet = freshSnippet firstSnippet
    , _timeLeft = time
    , _timeInitial = time
    , _currentScore = 0
    , _lifesLeft = l^.levelLifes
    , _level = l }
    where
        firstSnippet = head $ l^.levelSnippets
        time = calculateSnippetTime firstSnippet
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module HighScore where

import Lens.Micro.TH
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString as BS

data HighScore = HighScore { _playerName :: String, _score :: Int } deriving (Show, Eq, Generic)

makeLenses ''HighScore

instance Ord HighScore where
    compare (HighScore n1 s1) (HighScore n2 s2) = compare s1 s2 <> compare n1 n2

data HighScoreList = HighScoreList [HighScore] deriving (Show, Generic)

instance FromJSON HighScore
instance ToJSON HighScore

instance FromJSON HighScoreList
instance ToJSON HighScoreList

insertScore :: HighScore -> HighScoreList -> HighScoreList
insertScore score (HighScoreList scores) = HighScoreList $ take 10 $ insertInSortedList score scores

insertInSortedList :: Ord a => a -> [a] -> [a]
insertInSortedList x [] = [x]
insertInSortedList x (y:ys) = if x > y then x : y : ys else y : insertInSortedList x ys

readHighScores :: IO HighScoreList
readHighScores = do
    file <- BS.readFile "data/scores.json"
    let scores = decode (BS.fromStrict file) :: Maybe HighScoreList
    case scores of
        Just s -> return s
        Nothing -> error "Could not parse scores.json"

writeHighScores :: HighScoreList -> IO ()
writeHighScores scores = BS.writeFile "data/scores.json" $ BS.toStrict (encode scores)
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module HighScore where

import Lens.Micro.TH
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BS

data HighScore = HighScore { _playerName :: String, _score :: Int } deriving (Show, Eq, Generic)

makeLenses ''HighScore

instance Ord HighScore where
    compare (HighScore n1 s1) (HighScore n2 s2) = compare s1 s2 <> compare n1 n2

instance FromJSON HighScore
instance ToJSON HighScore

insertScore :: [HighScore] -> HighScore -> [HighScore]
insertScore scs sc = take 8 $ insertInSortedList sc scs

insertInSortedList :: Ord a => a -> [a] -> [a]
insertInSortedList x [] = [x]
insertInSortedList x (y:ys) = if x > y then x : y : ys else y : insertInSortedList x ys

readHighScores :: IO [HighScore]
readHighScores = do
    file <- BS.readFile "data/scores.json"
    let scores = decode file :: Maybe [HighScore]
    case scores of
        Just s -> return s
        Nothing -> error "Could not parse scores.json"

writeHighScores :: [HighScore] -> IO ()
writeHighScores scores = BS.writeFile "data/scores.json" (encode scores)
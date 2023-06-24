{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import Game
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import GHC.Generics
import Lens.Micro.TH

data Config = Config
    { _levels :: [Level]
    , _width :: Int
    , _height :: Int
    } deriving (Show, Generic)

makeLenses ''Config

instance FromJSON Config
instance ToJSON Config

readConfig :: IO Config
readConfig = do
    file <- BS.readFile "data/config.json"
    let config = decode file :: Maybe Config
    case config of
        Just c -> return c
        Nothing -> error "Could not parse config.json"


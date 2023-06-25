{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import Game
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import GHC.Generics
import Lens.Micro.TH

type Name = String

data Config = Config
    { _levels :: [Level]
    , _width :: Int
    , _height :: Int
    , _name :: Name
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

writeConfig :: Config -> IO ()
writeConfig config = BS.writeFile "data/config.json" $ encode config
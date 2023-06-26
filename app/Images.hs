{-# LANGUAGE TemplateHaskell #-}

module Images where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Lens.Micro.TH

data Images = Images
    { _monitorImg :: Picture
    , _keyboardImg :: Picture
    , _heartImg :: Picture
    , _brokenHeartImg :: Picture
    , _okImg :: Picture
    , _titleImg :: Picture
    , _scoresImg :: Picture
    , _nameImg :: Picture
    , _levelImg :: Picture
    }

makeLenses ''Images

loadImages :: IO Images
loadImages = do
    m <- fromMaybe blank <$> loadJuicyPNG "data/screen750x600.png"
    k <- fromMaybe blank <$> loadJuicyPNG "data/keyboard600x190.png"
    h <- fromMaybe blank <$> loadJuicyPNG "data/heart40.png"
    bh <- fromMaybe blank <$> loadJuicyPNG "data/broken_heart.png"
    o <- fromMaybe blank <$> loadJuicyPNG "data/ok.png"
    t <- fromMaybe blank <$> loadJuicyPNG "data/title.png"
    s <- fromMaybe blank <$> loadJuicyPNG "data/high_score.png"
    n <- fromMaybe blank <$> loadJuicyPNG "data/name.png"
    l <- fromMaybe blank <$> loadJuicyPNG "data/level.png"
    return
        Images
            { _monitorImg = m
            , _keyboardImg = k
            , _heartImg = h
            , _brokenHeartImg = bh
            , _okImg = o
            , _titleImg = t
            , _scoresImg = s
            , _nameImg = n
            , _levelImg = l
            }
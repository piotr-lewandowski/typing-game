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
    , _okImg :: Picture
    , _titleImg :: Picture
    }

makeLenses ''Images

loadImages :: IO Images
loadImages = do
    m <- fromMaybe blank <$> loadJuicyPNG "data/screen750x600.png"
    k <- fromMaybe blank <$> loadJuicyPNG "data/keyboard600x190.png"
    h <- fromMaybe blank <$> loadJuicyPNG "data/heart40.png"
    o <- fromMaybe blank <$> loadJuicyPNG "data/ok.png"
    t <- fromMaybe blank <$> loadJuicyPNG "data/title.png"
    return Images { _monitorImg = m, _keyboardImg = k, _heartImg = h, _okImg = o, _titleImg = t }
    where
        defaultHeart = color red $ circleSolid 40
        defaultOk = blank
        defaultMonitor = color black $ rectangleSolid 100 100
        defaultKeyboard = color black $ rectangleSolid 100 100
module Input where

import Graphics.Gloss.Interface.IO.Game

data InputEvents = GoUp | GoDown | Confirm | Typed Char | Resize (Int, Int) deriving (Show)

mapGlossEvents :: Event ->  Maybe InputEvents
mapGlossEvents (EventResize (w, h)) = Just $ Resize (w, h)
mapGlossEvents (EventKey (Char c) Down _ _) = Just $ Typed c
mapGlossEvents (EventKey (SpecialKey KeyUp) Down _ _) = Just GoUp
mapGlossEvents (EventKey (SpecialKey KeyDown) Down _ _) = Just GoDown
mapGlossEvents (EventKey (SpecialKey KeyEnter) Down _ _) = Just Confirm
mapGlossEvents (EventKey (SpecialKey KeySpace) Down _ _) = Just $ Typed ' '
mapGlossEvents _ = Nothing
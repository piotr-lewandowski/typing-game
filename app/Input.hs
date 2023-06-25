module Input(InputEvents(..), mapGlossEvents) where

import Graphics.Gloss.Interface.IO.Game
import qualified FRP.Yampa as Y
import InputEvents

mapGlossEvents :: Event -> Y.Event InputEvents
mapGlossEvents (EventResize (w, h)) = Y.Event $ Resize (w, h)
mapGlossEvents (EventKey (Char '\b') Down _ _) = Y.Event Backspace
mapGlossEvents (EventKey (Char c) Down _ _) = Y.Event $ Typed c
mapGlossEvents (EventKey (SpecialKey KeyUp) Down _ _) = Y.Event GoUp
mapGlossEvents (EventKey (SpecialKey KeyDown) Down _ _) = Y.Event GoDown
mapGlossEvents (EventKey (SpecialKey KeyEnter) Down _ _) = Y.Event Confirm
mapGlossEvents (EventKey (SpecialKey KeySpace) Down _ _) = Y.Event $ Typed ' '
mapGlossEvents (EventKey (SpecialKey KeyBackspace) Down _ _) = Y.Event Backspace
mapGlossEvents (EventKey (SpecialKey KeyEsc) Down _ _) = Y.Event Exit
mapGlossEvents _ = Y.NoEvent
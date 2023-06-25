module InputEvents where

data InputEvents = GoUp | GoDown | Confirm | Typed Char | Resize (Int, Int) | Backspace | Exit deriving (Show)

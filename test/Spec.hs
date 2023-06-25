
import Test.Hspec
import Test.QuickCheck
import Lens.Micro

import Gen

import GameSpec

main :: IO ()
main = do
    gameTests
    

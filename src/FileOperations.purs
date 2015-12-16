module FileOperations where

import Prelude

import Data.Path
import Data.Array

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven x = isEven (x - 2)

countEven :: Array Int -> Int
countEven = length <<< filter isEven

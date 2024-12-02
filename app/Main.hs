module Main (main) where

import Lib

main :: IO ()
main = do
  mapM_ run [1..25]

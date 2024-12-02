module Main (main) where

import Lib

main :: IO ()
main = do
  run 2
  -- mapM_ run [1..25]

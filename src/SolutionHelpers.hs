{-# LANGUAGE ScopedTypeVariables #-}
module SolutionHelpers where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L


toTuple :: [α] -> (α, α)
toTuple [a, b] = (a, b)

quicksort :: Ord α => [α] -> [α]
quicksort [] = []
quicksort [a] = [a]
quicksort (x: xs) = quicksort la ++ (x : quicksort lb)
  where (la, lb) = foldl f ([], []) xs
          where f (la, lb) e | e < x = (e:la, lb)
                             | otherwise = (la, e:lb)
                  
uniq :: Eq α => [α] -> [α]
uniq = foldl (\acc e -> if e `elem` acc
                        then acc
                        else e:acc) []

count :: (Eq a, Hashable a) => [a] -> H.HashMap a Int         
count = H.fromList . map (\l@(x:xs) -> (x, length l)) . L.group

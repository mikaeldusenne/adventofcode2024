{-# LANGUAGE ScopedTypeVariables #-}
module SolutionHelpers where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import Data.List (foldl')

toDf :: String -> [[Int]]
toDf = map (map (read :: String -> Int) . words) . lines

toTuple :: [α] -> (α, α)
toTuple [a, b] = (a, b)

quicksort :: Ord α => [α] -> [α]
quicksort [] = []
quicksort [a] = [a]
quicksort (x: xs) = quicksort la ++ (x : quicksort lb)
  where (la, lb) = foldl' f ([], []) xs
          where f (la, lb) e | e < x = (e:la, lb)
                             | otherwise = (la, e:lb)
                  
uniq :: Eq α => [α] -> [α]
uniq = foldr (\e acc -> if e `elem` acc
                        then acc
                        else e:acc) []

count :: (Eq a, Hashable a) => [a] -> H.HashMap a Int         
count = H.fromList . map (\l@(x:xs) -> (x, length l)) . L.group

sign a | a < 0 = -1
       | otherwise = 1


oneoffs :: [Int] -> [[Int]]
oneoffs = run [] []
  where run :: [[Int]] -> [Int] -> [Int] -> [[Int]]
        run acc _ [] = acc
        run acc left (x:xs) = run ((left ++ xs):acc) (left++[x]) xs


spann :: Eq α => [α] -> [α] -> ([α], [α])
spann s = run []
  where run ans [] = (reverse ans, [])
        run ans l'@(x:xs) | s `L.isPrefixOf` l' = (reverse ans, l')
                   | otherwise = run (x:ans) xs


spann' :: Eq α => ([α] -> Bool) -> [α] -> ([α], [α])
spann' f = run []
  where run ans [] = (reverse ans, [])
        run ans l'@(x:xs) | f l' = (reverse ans, l')
                   | otherwise = run (x:ans) xs


splitOn :: Eq α => [α] -> [α] -> [[α]]
splitOn _ [] = []
splitOn sep l = a : splitOn sep (drop (length sep) b)
  where (a, b) = spann sep l
  

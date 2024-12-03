{-# LANGUAGE ScopedTypeVariables #-}
module Lib (
  solution,
  Solution,
  ) where

import qualified Data.List as L
import qualified Data.HashMap.Strict as H
import SolutionHelpers (toTuple, quicksort, uniq, count, toDf, sign, oneoffs, splitOn)
import Data.Either (rights)

type Solution = (Maybe Int, Maybe Int)

solution :: Int -> String -> Either String Solution
solution 1 s = Right (Just solution_a, Just solution_b)
  where (la, lb) :: ([Int], [Int]) = unzip . map (toTuple . map (read :: String -> Int) . words) . lines $ s
        (sortedla, sortedlb) = (quicksort la, quicksort lb)
        solution_a = sum $ zipWith (\ a b -> abs (a - b)) sortedla sortedlb
      
        la1 = uniq sortedla
        lb1 = uniq sortedlb
        m = count $ filter (`elem` la1) sortedlb
        solution_b = sum . map (\e -> (e * (m H.! e))) $ L.intersect la1 lb1
  
solution 2 s = Right (Just solution_a, Just solution_b)
  
  where df = toDf s
        isOk :: [Int] -> Bool
        isOk l = fst . foldr fok (True, 0) $ zipWith (-) (l) (tail l)
          where fok e (False, sgn) = (False, sgn)
                fok e (True, sgn) | (sgn == 0 || (sign e == sgn)) && abs e >= 1 && abs e <= 3 = (True, sign e)
                                  | otherwise = (False, sgn)
                                
        solution_a = length . filter id . map isOk $ df

        isOk' :: [Int] -> Bool
        isOk' = any isOk . oneoffs

        solution_b = length . filter id . map isOk' $ df
        
solution 3 s = Right (solution_a, solution_b)
          
  where f s = case (reads s :: [((Int, Int), String)]) of
                [] -> Left ""
                [(a, _)] -> Right a
                l -> Left $ show l
        sol = sum . map (uncurry (*)) . rights . map f . splitOn "mul"
        f' = map ((!!0) . splitOn "don't") . splitOn "do()"

        solution_a = Just . sol $ s
        solution_b = Just . sol . unwords . f' $ s

solution n _ = Left $ "Problem " ++ show n ++ " not yet implemented !"


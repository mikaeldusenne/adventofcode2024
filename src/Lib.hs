{-# LANGUAGE ScopedTypeVariables #-}
module Lib (
  solution
  ) where

import qualified Data.List as L
import qualified Data.HashMap.Strict as H
import SolutionHelpers (toTuple, quicksort, uniq, count, toDf, sign, oneoffs)

  
solution :: Int -> String -> Either String (Maybe Int, Maybe Int)
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

  
solution n _ = Left $ "Problem " ++ show n ++ " not yet implemented !"

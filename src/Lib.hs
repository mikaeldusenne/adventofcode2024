{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Lib
    ( run
    ) where

import System.Environment (getEnv)
import qualified Data.List as L
import System.FilePath ((</>))
import Control.Exception (try, IOException)
import qualified Data.HashMap.Strict as H
import SolutionHelpers (toTuple, quicksort, uniq, count, toDf, sign)


putSolution :: Show a => String -> a -> IO()
putSolution p = putStrLn . (("Solution " ++ p ++ " : ")++) . show

loadData :: Int -> IO String
loadData n = (>>=readFile) $ (</>(show n++".csv")) <$> getEnv "ADVENTCALENDAR2024_DATA_DIR" 

run :: Int -> IO()
run n = try @IOException  (loadData n) >>= \case
    Left _ -> putStrLn ("No data for problem " ++ show n)
    Right s -> solution n s

  
solution :: Int -> String -> IO ()
solution 1 s = do
  let (la, lb) :: ([Int], [Int]) = unzip . map (toTuple . map (read :: String -> Int) . words) . lines $ s
  let (sortedla, sortedlb) = (quicksort la, quicksort lb)
      
  putSolution "1a" $ sum $ zipWith (\ a b -> abs (a - b)) sortedla sortedlb
  
  let la1 = uniq sortedla
      lb1 = uniq sortedlb
      m = count $ filter (`elem` la1) sortedlb
      
  putSolution "1b" $ sum . map (\e -> (e * (m H.! e))) $ L.intersect la1 lb1

  
solution 2 s = do
  
  let df = toDf s
      isOk :: [Int] -> Bool
      isOk l = fst . foldr fok (True, 0) $ zipWith (-) (l) (tail l)
        where fok e (False, sgn) = (False, sgn)
              fok e (True, sgn) | (sgn == 0 || (sign e == sgn)) && abs e >= 1 && abs e <= 3 = (True, sign e)
                                | otherwise = (False, sgn)
                                
      -- isOk l = all $ (<*>l) [isMonotonic]
      --   where isMonotonic l = foldr (\e acc -> ) 0
  putSolution "2a" $ length . filter id . map isOk $ df
  
  
solution n _ = putStrLn $ "Problem " ++ show n ++ " not yet implemented !"

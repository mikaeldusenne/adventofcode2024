{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Lib
    ( run
    ) where

import System.Environment (getEnv)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import Data.Hashable (Hashable)
import System.FilePath ((</>))
import Control.Exception (try, IOException)

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

putSolution :: Show a => String -> a -> IO()
putSolution p = putStrLn . (("Solution " ++ p ++ " : ")++) . show

loadData :: Int -> IO String
loadData n = (>>=readFile) $ (</>(show n++".csv")) <$> getEnv "ADVENTCALENDAR2024_DATA_DIR" 

run :: Int -> IO()
run n = try @IOException  (loadData n) >>= \case
    Left _ -> putStrLn ("No data for problem " ++ show n)
    Right s -> case n of
      1 -> do
        let (la, lb) :: ([Int], [Int]) = unzip . map (toTuple . map (read :: String -> Int) . words) . lines $ s
        let (sortedla, sortedlb) = (quicksort la, quicksort lb)
  
        putSolution "1a" $ sum $ zipWith (\ a b -> abs (a - b)) sortedla sortedlb
  
        let la1 = uniq sortedla
            lb1 = uniq sortedlb
            m = count $ filter (`elem` la1) sortedlb
      
        putSolution "1b" $ sum . map (\e -> (e * (m H.! e))) $ L.intersect la1 lb1
      
      n -> putStrLn $ "Problem " ++ show n ++ " not yet implemented !"

  

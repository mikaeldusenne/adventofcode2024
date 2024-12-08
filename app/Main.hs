{-# LANGUAGE ScopedTypeVariables, LambdaCase, BangPatterns, TupleSections #-}
module Main (main) where

import Lib (Solution, solution)
import Control.Exception (try, IOException)
import System.FilePath ((</>))
import System.Environment (getEnv)
import Data.List (intercalate)
import SolutionHelpers
import Data.Either (rights)
import qualified Data.List as L
import Data.List (foldl')
import qualified Data.HashMap.Strict as H

putSolution :: Show a => String -> a -> IO()
putSolution p = putStrLn . (("Solution " ++ p ++ " : ")++) . show

loadData :: Int -> IO String
loadData n = (>>=readFile) $ (</>(show n++".csv")) <$> getEnv "ADVENTCALENDAR2024_DATA_DIR" 

tupleToList (a, b) = [a, b]

run :: Int -> IO String
run n  = try @IOException  (loadData n) >>= \case
    Left _ -> return ("No data for problem " ++ show n)
    Right s -> case solution n s of
      Left err -> return err
      Right result -> return $ init . unlines $ zipWith showResult "ab" (tupleToList result)
        where showResult k val = let prettyprob = "problem " ++ show n ++ [k] ++ " : " in 
                case val of (Just e) ->  prettyprob ++ show e
                            Nothing ->  prettyprob ++ "not implemented yet"


main :: IO ()
main = do
  let sep = "\n────────────────────────────────\n"
  mapM run [1..25] >>= (putStrLn . (sep++) .(++sep) . intercalate sep)
  -- run 8  >>= putStrLn
  -- s <- loadData 8
  
  print "done."


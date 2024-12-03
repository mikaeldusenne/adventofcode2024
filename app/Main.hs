{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Main (main) where

import Lib (solution)
import Control.Exception (try, IOException)
import System.FilePath ((</>))
import System.Environment (getEnv)
import Data.List (intercalate)

putSolution :: Show a => String -> a -> IO()
putSolution p = putStrLn . (("Solution " ++ p ++ " : ")++) . show

loadData :: Int -> IO String
loadData n = (>>=readFile) $ (</>(show n++".csv")) <$> getEnv "ADVENTCALENDAR2024_DATA_DIR" 

tupleToList (a, b) = [a, b]

run :: Int -> IO String
run n = try @IOException  (loadData n) >>= \case
    Left _ -> return ("No data for problem " ++ show n)
    Right s -> case solution n s of
      Left err -> return err
      Right result -> return $ init . unlines $ zipWith showResult "ab" (tupleToList result)
        where showResult k val = let prettyprob = "problem " ++ show n ++ [k] ++ " : " in 
                case val of (Just e) ->  prettyprob ++ show e
                            Nothing ->  prettyprob ++ " not implemented yet"
                

main :: IO ()
main = do
  let sep = "\n────────────────────────────────\n"
  mapM run [1..25] >>= (putStrLn . (sep++) .(++sep) . intercalate sep)

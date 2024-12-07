{-# LANGUAGE ScopedTypeVariables #-}
module Lib (
  solution,
  Solution,
  ) where

import qualified Data.List as L
import qualified Data.HashMap.Strict as H
import SolutionHelpers
import Data.Either (rights)
import Data.List (foldl')
import Control.Parallel.Strategies

type Solution = (Maybe Int, Maybe Int)

data Position = Position {x :: Int, y :: Int}
  deriving (Show, Eq)

data Direction = DD | DR | DU | DL
  deriving (Show, Eq, Ord, Enum, Bounded)

data PosDir = PosDir {pos :: Position, dir :: Direction}
  deriving (Show, Eq)

dx :: Direction -> Int
dx DU = 0
dx DR = 1
dx DD = 0
dx DL = -1

dy :: Direction -> Int
dy DU = -1
dy DR = 0
dy DD = 1
dy DL = 0


rotate :: Direction -> Direction
rotate d = if d == maxBound 
           then minBound 
           else succ d
                
rotatePosDir PosDir{dir=dir, pos=pos} = PosDir{dir=rotate dir, pos=pos}

data Reason = Exit | Loop
  deriving (Show, Eq)

data Path = Path{reason::Reason, path::[PosDir]}
  deriving (Show, Eq)



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

solution 4 s = Right (Just solution_a, Just solution_b)
  where w = "XMAS"
        countOccurences _ [] = 0
        countOccurences w l@(x:xs) | w `L.isPrefixOf` l = 1 + countOccurences w (drop (length w) l)
                                   | otherwise = countOccurences w (dropWhile (\e -> e /= head w) xs)

        countRev l = countOccurences w l + countOccurences (reverse w) l

        -- fma :: (α -> α) -> α -> [α]
        fma f l = [l, f l]

        fs :: [[String] -> [[String]]]
        -- fs = map fma [diags, transpose, map reverse]
        fs = map fma [transpose, diags]
        
        revs :: [String] -> [[String]]
        revs l = foldl' (flip flatMap) [l] fs
  
        combs = [
          id
          , transpose
          , diags
          , diags . map reverse
          ]
        
        solution_a = sum $ map (sum . map countRev ) $ combs <*> pure (lines s)
        
        ls = lines s
        width = length (head ls)
        height = length ls

        prepare :: String -> [Int]
        prepare = replaceDefault 0 (H.fromList [('A', 9), ('M', 1), ('S', 2)])
                  . filter (/= '\n')
        prepared = prepare s
        
        countXMAS :: Int -> Int -> [Int] -> Int
        countXMAS w h v = length . filter neighboursOk
          . filter (not . isBorder)
          . findIndices 9
          $ v
          where isBorder i = ((i `div` w) `elem` [0, w-1])
                              || ((i `mod` w) `elem` [0, h-1])
                             
                coords i = ((i `div` w), (i `mod` w), isBorder i)
                
                neighboursOk :: Int -> Bool
                neighboursOk i = all ((==3) . sum) $ neighbours i
                
                neighbours :: Int -> [[Int]]
                neighbours i = map (map (prepared !!)) $ [[i-(w+1), i+(w+1)], [i-(w-1), i+(w-1)]]
        solution_b = countXMAS width height prepared



solution 5 s = Right (Just solution_a, Just solution_b)
  where [r, m] = splitOn "\n\n" s
        fread :: Char -> String -> [[String]]
        fread c = map (split c) . lines
        rules :: [[String]]
        rules = fread '|' r
        manuals = fread ',' m
        isOk [] = True
        isOk (x:xs) = ok && isOk xs
          where ok = none (\[a, b] -> (b==x) && (a `elem`xs)) rules
        midPage l = l !! ((`div`2) . length $ l)
        okManuals = filter isOk $ manuals
        notOkManuals = filter (not . isOk) manuals
        insertAfter a b [] = [b]
        insertAfter a b (x:xs) | x == a = x:b:xs
                               | otherwise = x:insertAfter a b xs
                               
        sortManual :: [String] -> [String]
        sortManual [] = []
        sortManual l@(x:xs) = case match of
          Nothing -> (x : sortManual xs)
          Just e -> (sortManual $ insertAfter e x xs)
          where match :: Maybe String
                match = (head<$>) .safeHead . filter (\[a, b] -> (b==x) && (a`elem`xs)) $ rules

        solution_a = sum . map (readInt . midPage) $ okManuals
        solution_b = sum . map (readInt . midPage) $ map sortManual $ notOkManuals


solution 6 s = Right (Just solution_a, Just solution_b)
  where l :: [[Char]]
        l = lines s
        h = length l
        w = length $ head l
        ss = filter (/='\n') $ s
        board = filter ((/='.') . snd) . flatten . iterWith (\row l -> iterWith (\col c -> ((row, col), c)) l) $ l
  
        startPos :: PosDir
        startPos = PosDir (uncurry Position . fst . head . filter ((=='^') . snd) $ board) DL
        
        obst :: [Position]
        obst = map (uncurry Position . fst) . filter ((=='#') . snd) $ board
  
        walk :: [Position] -> Path
        walk obst = move [startPos]
          where move :: [PosDir] -> Path
                move l@(p@PosDir{pos=ppp@Position{x=x, y=y}, dir=dir}:ps)
                  | isOut = Path Exit ps
                  | isLoop = Path Loop l
                  | isObst = move ((rotatePosDir $ head ps):tail ps)
                  | otherwise = move (PosDir nextPos dir:l)
                  where isOut = (x<=0) || (x>w) || (y<=0) || (y>h)
                        nextPos = Position (x + dx dir) (y + dy dir)
                        nextDir = rotate dir
                        isObst = ppp `elem` obst
                        isLoop = p `elem` ps
        
        fill :: [Position] -> Char -> [[Char]] -> [[Char]]
        fill ps c = iterWith (\row l -> iterWith (\col cc -> if (Position{x=row, y=col}`elem`ps) then c else cc) l)
      
        p@Path{reason=r, path=path} = walk obst
      
        solution_a = length $ uniq . map pos $ path
        possibleNewObstacles = uniq $ filter (\e -> e /= pos startPos) . map pos $ path
        -- this is too computexpensive
        loopObsts = filter ((==Loop).reason) $ parMap rseq (walk . (:obst)) possibleNewObstacles
        
        solution_b = length loopObsts

solution 7 s = Right (Just solution_a, Just solution_b)
  where l :: [(Int, [Int])]
        l = map (headNtail . map readInt . words . filter (/=':')) . lines $ s
        ops = [(*), (+)]
        
        search ops sol (x:xs) = go x xs
          where go total [] = total == sol
                go total (x:xs) = any (\o -> go (o total x) xs) $ ops
                
        ops2 = (\a b -> readInt (show a ++ show b)) : ops
        runops os = sum . map fst $ filter (uncurry (search os)) $ l
        
        solution_a = runops ops
        solution_b = runops ops2


solution n _ = Left $ "Problem " ++ show n ++ " not yet implemented !"


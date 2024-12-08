{-# LANGUAGE ScopedTypeVariables #-}
module SolutionHelpers where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import Data.List (foldl')

type Coord = (Int, Int)
type BoardPiece = (Coord, Char)

data Pos = Pos Int Int
  deriving (Show, Eq)

instance Num Pos where
  (Pos x1 y1) + (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)
  (Pos x1 y1) * (Pos x2 y2) = Pos (x1 * x2) (y1 * y2)
  negate (Pos x y) = Pos (-x) (-y)
  abs (Pos x y) = Pos (abs x) (abs y)
  signum (Pos x y) = Pos (signum x) (signum y)
  -- fromInteger n = Pos (fromInteger n) 0

posToTuple (Pos x y) = (x, y)

readInt :: String -> Int
readInt = read

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


split :: (Eq α) => α -> [α] -> [[α]]
split _ [] = []
split sep l =
  let (a,b) = break (==sep) l in
  case b of
    [] -> [a]
    _ -> a : (split sep $ tail b)

splitOn :: Eq α => [α] -> [α] -> [[α]]
splitOn _ [] = []
splitOn sep l = a : splitOn sep (drop (length sep) b)
  where (a, b) = spann sep l
  

flatMap :: (α -> [β]) -> [α] -> [β]
flatMap = (concat .) . map

-- down right
diags l = fvert (tail l) ++ fhoriz l
  where f = map head . filter (not . null) . zipWith drop [0..]
        fhoriz [] = []
        fhoriz l = f l : fhoriz (filter (not . null) . map tail $ l)
        fvert [] = []
        fvert l = f l : fvert (tail l)

transpose :: [[α]] -> [[α]]
transpose ([]:_) = []
transpose l = a : transpose b
  where tuplecons (a,b) (c,d) = (a:c , b:d)
        -- (a,b) = foldr tuplecons ([],[]) $ map headNtail $ l
        (a,b) = foldr (tuplecons . headNtail) ([],[]) l

headNtail (x:xs) = (x,xs)

replaceDefault z m = go []
  where go acc [] = reverse acc
        go acc (x:xs) = go (H.lookupDefault z x m : acc) xs

replace a b = map (\x -> if x == a then b else x)

findIndices x xs = map snd $ filter ((==x) . fst) $ zip xs [0..]

safeHead [] = Nothing
safeHead (x:_) = Just x

headOrDefault d [] = d
headOrDefault _ (x:_) = x

none :: (Foldable f) => (α -> Bool) -> f α -> Bool
none = (not.) . any


reduce :: (a -> a -> a) -> [a] -> a
reduce _ [] = error "reduce empty list"
reduce _ [_] = error "reduce single element"
reduce f l = foldl' f (head l) (tail l)

flatten :: [[a]] -> [a]
flatten = reduce (++)

iter :: [b] -> [(Int, b)]
iter = zip [1..]

iterWith :: (Int -> α -> c) -> [α] -> [c]
iterWith = ($ [1..]) . zipWith 

boardPositions :: [Char] -> [Char] -> [((Int, Int), Char)]
boardPositions ignore = filter (not . (`elem`ignore) . snd) . flatten . iterWith (\row l -> iterWith (\col c -> ((row, col), c)) l) . lines


-- isInBoard size (a, b) = (a>=0) && (a<size) && (b>=0) && (b<size)
isInBoard size (a, b) = (a>0) && (a<=size) && (b>0) && (b<=size)

drawBoard :: Int -> Char -> [BoardPiece] -> [Char]
drawBoard size filler pieces = unlines (
  map (\row -> map (\col -> pieceAt (row, col)) [0..size])
  [0..size]
  )
  where repn = replicate size
        pieceAt p = headOrDefault filler . map snd . filter ((==p) . fst) $ pieces


append = flip (++) . (:[])

groupBy :: (Eq b,Ord a) => (a -> b) -> [a] -> [[a]]
groupBy f = foldl' insert []
  where insert acc e = walk acc
          where walk [] = [[e]]
                walk (l@(x:_):ls) | f x == f e = append e l : ls
                                  | otherwise = l : walk ls

tails :: [α] -> [[α]]
tails [] = []
tails l@(_:xs) = l : tails xs

pairs :: [α] -> [(α, α)]
pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

recurse f start = start : recurse f (f start)

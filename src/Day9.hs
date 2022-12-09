module Day9
    ( day9
    ) where

import Data.List (nub)
import Data.List.Split (splitOn)

data Motion = R Int | U Int | L Int | D Int
    deriving (Show)

type Step = (Int, Int)

type Position = (Int, Int)

type Rope = [Position]

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> [Motion]
parseLines = map parse
    where
        parse :: String -> Motion
        parse x = parse' $ splitOn " " x
        parse' :: [String] -> Motion
        parse' [a, b] = parse'' a (read b :: Int)
        parse' _ = error "bad input"
        parse'' :: String -> Int -> Motion
        parse'' "R" a = R a
        parse'' "U" a = U a
        parse'' "L" a = L a
        parse'' "D" a = D a
        parse'' _ _ = error "bad input"

steps :: [Motion] -> [Step]
-- break each motion down into individual steps
steps xs = concatMap steps' xs
    where
        steps' :: Motion -> [Step]
        steps' (R a) = replicate a (1, 0)
        steps' (U a) = replicate a (0, 1)
        steps' (L a) = replicate a (-1, 0)
        steps' (D a) = replicate a (0, -1)

bring :: Position -> Position -> Position
-- this is where a single knot brings the next knot along with it
bring (nx, ny) (x, y)
    | (abs (nx - x) < 2) && (abs (ny - y) < 2) = (x, y)
    | nx > x && ny == y = (x + 1, y)
    | nx < x && ny == y = (x - 1, y)
    | nx == x && ny > y = (x, y + 1)
    | nx == x && ny < y = (x, y - 1)
    | nx > x && ny > y = (x + 1, y + 1)
    | nx > x && ny < y = (x + 1, y - 1)
    | nx < x && ny > y = (x - 1, y + 1)
    | nx < x && ny < y = (x - 1, y - 1)
    | otherwise = error "cannot occur"

pull :: Rope -> Step -> Rope
-- this is where a single step is applied to the entire rope
pull ((x, y) : ps) (dx, dy) = pull' [(x + dx, y + dy)] ps
    where
        pull' :: Rope -> Rope -> Rope
        pull' acc [] = reverse acc
        pull' (np : nps) (op : ops) = pull' ((bring np op) : np : nps) ops
        pull' [] _ = error "cannot occur"
pull [] _ = error "cannot occur"
 
record :: Rope -> [Step] -> [Position]
-- this records the path of the last knot as the steps are applied
record = record' []
    where
        record' :: [Position] -> Rope -> [Step] -> [Position]
        record' acc _ [] = reverse acc
        record' acc r (s : ss) = record'' $ pull r s
            where
                record'' :: Rope -> [Position]
                record'' nr = record' ((last nr) : acc) nr ss

day9 :: IO ()
day9 = do
    xs <- steps <$> parseLines <$> slurpLines "day9.txt"
    let knots = replicate 2 (0, 0)
    let r1 = length $ nub $ record knots xs
    putStrLn ("Day 9, part 1: " ++ (show r1))
    let tenKnots = replicate 10 (0, 0)
    let r2 = length $ nub $ record tenKnots xs
    putStrLn ("Day 9, part 2: " ++ (show r2))

module Day15
    ( day15
    ) where

import Data.List
import Data.Char (isDigit)
import Data.List.Split
import Data.Maybe

type Coordinate = (Int, Int)

type Range = (Int, Int)

data Sensor = Sensor {
    position :: Coordinate,
    beacon :: Coordinate
} deriving (Show)

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

toInt :: String -> Int
toInt cs = read cs :: Int

parseLines :: [String] -> [Sensor]
parseLines xs = map (parse . wordsBy (==' ') . filter wheat) xs
    where
        wheat :: Char -> Bool
        wheat '-' = True
        wheat ' ' = True
        wheat c = isDigit c
        parse :: [String] -> Sensor
        parse css = parse' $ map toInt css
        parse' :: [Int] -> Sensor
        parse' (px : py : bx : by : _) = Sensor (px, py) (bx, by)

manhattan :: Coordinate -> Coordinate -> Int
manhattan (a, b) (c, d) = abs (a - c) + abs (b - d)

rangeAtY :: Sensor -> Int -> Maybe Range
rangeAtY (Sensor (px, py) (bx, by)) y = rangeAtY' (manhattan (px, py) (bx, by)) (abs (y - py))
    where
        rangeAtY' :: Int -> Int -> Maybe Range
        rangeAtY' md yd
            | md >= yd = Just (px - (md - yd), px + (md - yd))
            | otherwise = Nothing

rangesAtY :: [Sensor] -> Int -> [Range]
rangesAtY xs y = catMaybes $ foldl (\ a b -> (rangeAtY b y) : a) [] xs

squash :: [Range] -> [Range]
squash xs = squash' $ filter (\ x -> length x == 2) $ subsequences xs
    where
        squash' :: [[Range]] -> [Range]
        squash' [] = sort xs
        squash' ((a : b : _) : cs)
            | overlap a b = squash (combine a b : (xs \\ [a, b]))
            | otherwise = squash' cs

overlap :: Range -> Range -> Bool
overlap (a, b) (c, d)
    | (a + 1) >= c && (a - 1) <= d = True
    | (b + 1) >= c && (b - 1) <= d = True
    | (c + 1) >= a && (c - 1) <= b = True
    | (d + 1) >= a && (d - 1) <= b = True
    | otherwise = False

combine :: Range -> Range -> Range
combine (a, b) (c, d) = (min a c, max b d)

numBeaconsAt :: [Sensor] -> Int -> Int
numBeaconsAt xs y = length $ nub $ map (\ x -> fst (beacon x)) $ filter (\ x -> snd (beacon x) == y) xs

numPositions :: [Range] -> Int
numPositions = numPositions' 0
    where
        numPositions' :: Int -> [Range] -> Int
        numPositions' acc [] = acc
        numPositions' acc (r : rs) = numPositions' (acc + 1 + snd r - fst r) rs

sweep :: [Sensor] -> Int -> Coordinate
sweep sensors maxXOrY = sweep' 0
    where
        sweep' :: Int -> Coordinate
        sweep' y
            | y > maxXOrY = error "overrun"
            | otherwise = sweep'' $ squash (rangesAtY sensors y)
            where
                sweep'' :: [Range] -> Coordinate
                sweep'' (a : b : _) | snd a + 2 == fst b = (snd a + 1, y)
                sweep'' _ = sweep' (y + 1)

day15 :: IO ()
day15 = do
    sensors <- fmap parseLines $ slurpLines "day15.txt"
    let yLevel = 2000000
    let ranges = squash (rangesAtY sensors yLevel)
    let r1 = numPositions ranges - (numBeaconsAt sensors yLevel)
    putStrLn ("Day 15, part 1: " ++ (show r1))
    let c = sweep sensors 4000000
    let r2 = ((fst c) * 4000000) + (snd c)
    putStrLn ("Day 15, part 2: " ++ (show r2))

module Day12
    ( day12
    ) where

import Data.List (sort)
import qualified Data.Map as Map
import Data.Char (ord)

type Coordinate = (Int, Int)

type Elevation = Char

type Grid = Map.Map Coordinate Elevation

type DistanceMap = Map.Map Coordinate Int

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> Grid
parseLines xs = Map.fromList $ concatMap parseLines' $ zip [0..] xs
    where
        parseLines' :: (Int, String) -> [((Int, Int), Char)]
        parseLines' (y, cs) = map (\ (x, c) -> ((x, y), c)) $ zip [0..] cs

bounds :: Grid -> Coordinate
bounds g = head $ reverse $ sort $ Map.keys g

findByDistance :: DistanceMap -> Int ->  [Coordinate]
findByDistance dm n = map fst $ filter (\ ((_, _), d) -> d == n) $ Map.toList dm 

passable :: Grid -> Coordinate -> Coordinate -> Bool
passable g c d = passable' (g Map.! c) (g Map.! d)
    where
        passable' :: Elevation -> Elevation -> Bool 
        passable' a b
            | a == 'S' = (b == 'a' || b == 'b')
            | b == 'S' = (a == 'a' || a == 'b')
            | b == 'E' = (a == 'z' || a == 'y')
            | a >= b = True
            | (ord a) + 1 == (ord b) = True
            | otherwise = False

paths :: Grid -> Coordinate -> Coordinate -> [Coordinate]
paths g (bx, by) (x, y) = filter (\ c -> passable g (x, y) c) $ filter inbounds rose
    where
        inbounds :: (Int, Int) -> Bool
        inbounds (a, b) = (a >= 0) && (b >= 0) && (a <= bx) && (b <= by)
        rose :: [Coordinate]
        rose = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

step :: Grid -> Coordinate -> DistanceMap -> Int -> DistanceMap
step g b dm n = step' $ concatMap (\ c -> paths g b c) (findByDistance dm n)
    where
        step' :: [Coordinate] -> DistanceMap
        step' cs = foldl step'' dm cs
        step'' :: DistanceMap -> Coordinate -> DistanceMap
        step'' m c
            | (Map.notMember c m) = Map.insert c (n + 1) m
            | otherwise = m         

findByElevation :: Grid -> Char -> [Coordinate]
findByElevation g c = map fst $ filter (\ (_, b) -> b == c) $ Map.toList g

fill :: Grid -> Coordinate -> DistanceMap -> DistanceMap
fill g b dm = foldl (\ mm nn -> step g b mm nn) dm [0..((fst b * snd b) + 2)]

day12 :: IO ()
day12 = do
    g <- parseLines <$> slurpLines "day12.txt"
    let b = bounds g
    let e = head $ findByElevation g 'E'
    let s = head $ findByElevation g 'S'
    let dm1 = fill g b $ Map.fromList [(s, 0)]
    let r1 = dm1 Map.! e
    putStrLn ("Day 12, part 1: " ++ (show r1))
    let starts = s : (findByElevation g 'a')
    let dm2 = fill g b $ Map.fromList $ map (\ c -> (c, 0)) starts
    let r2 = dm2 Map.! e
    putStrLn ("Day 12, part 2: " ++ (show r2))

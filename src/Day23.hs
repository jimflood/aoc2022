module Day23
    ( day23
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List (groupBy, sortOn)

type Coordinate = (Int, Int)

type CoordinateSet = Set.Set Coordinate

type Move = (Coordinate, Coordinate)

data Direction = North | South | West | East
    deriving (Show)

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseGrid :: [String] -> [(Coordinate, Char)]
parseGrid css = concatMap (\ (y, cs) -> map (\ (x, c) -> ((x, y), c)) (zip [0..] cs)) $ zip [0..] css

parseLines :: [String] -> CoordinateSet
parseLines css = Set.fromList $ map fst $ filter (\ (xy, c) -> c == '#') $ parseGrid css

viewing :: Coordinate -> Direction -> CoordinateSet
viewing (x, y) d = Set.fromList $ map (\ (dx, dy) -> (x + dx, y + dy)) $ viewing' d
    where
        viewing' :: Direction -> [Coordinate]
        viewing' North = [(-1, -1), (0, -1), (1, -1)]
        viewing' South = [(-1, 1), (0, 1), (1, 1)]
        viewing' West = [(-1, -1), (-1, 0), (-1, 1)]
        viewing' East = [(1, -1), (1, 0), (1, 1)]

available :: CoordinateSet -> Coordinate -> Direction -> Bool
available cs xy d = Set.null $ Set.intersection cs (viewing xy d)

moving :: Coordinate -> Direction -> Coordinate
moving (x, y) = moving'
    where
        moving' North = (x, y - 1)
        moving' South = (x, y + 1)
        moving' West = (x - 1, y)
        moving' East = (x + 1, y)

move :: CoordinateSet -> Coordinate -> [Direction] -> Maybe Move
move cs xy [] = Nothing
move cs xy (a : b : c : d : _) | all (\ a -> available cs xy a) [a, b, c, d] = Nothing
move cs xy (d : ds)
    | available cs xy d = Just (xy, (moving xy d))
    | otherwise = move cs xy ds 

firstHalf :: CoordinateSet -> [Direction] -> [Move]
firstHalf cs ds = mapMaybe (\ xy -> move cs xy ds) (Set.toList cs)

secondHalf :: CoordinateSet -> [Move] -> CoordinateSet
secondHalf cs ms = Set.union (Set.difference cs froms) tos
    where
        froms :: CoordinateSet
        froms = Set.fromList $ map fst solos
        tos :: CoordinateSet
        tos = Set.fromList $ map snd solos
        solos :: [Move]
        solos = map head $ filter (\ a -> length a == 1) (groupBy (\ a b -> snd a == snd b) (sortOn snd ms))

step :: (CoordinateSet, [Direction]) -> (CoordinateSet, [Direction])
step (cs, ds) = (secondHalf cs (firstHalf cs ds), rotate ds)
    where
        rotate :: [Direction] -> [Direction]
        rotate xs = tail xs ++ [head xs]

bounds :: CoordinateSet -> (Coordinate, Coordinate)
bounds cs = ((minX, minY), (maxX, maxY))
    where
        xys = Set.toList cs
        minX = minimum (map fst xys)
        maxX = maximum (map fst xys)
        minY = minimum (map snd xys)
        maxY = maximum (map snd xys)

area :: CoordinateSet -> Int
area cs = area' $ bounds cs
    where
        area' :: (Coordinate, Coordinate) -> Int
        area' ((minX, minY), (maxX, maxY)) = (1 + maxX - minX) * (1 + maxY - minY)

{-}
display :: CoordinateSet -> IO ()
display cs = display' [minY..maxY]
    where
        xys = Set.toList cs
        minX = minimum (map fst xys)
        maxX = maximum (map fst xys)
        minY = minimum (map snd xys)
        maxY = maximum (map snd xys)
        display' [] = return ()
        display' (y : ys) = do
            display'' [minX..maxX]
            display' ys
            where
                display'' [] = putStrLn ""
                display'' (x : xs) = do
                    display''' (x, y)
                    display'' xs
                    where
                        display''' xy
                            | Set.member xy cs = putStr("#")
                            | otherwise = putStr(".")
-}

run :: (CoordinateSet, [Direction]) -> Int
run = run' 1
    where
        run' :: Int -> (CoordinateSet, [Direction]) -> Int
        run' n (cs, ds) = run'' (step (cs, ds))
            where
                run'' :: (CoordinateSet, [Direction]) -> Int
                run'' (cs1, ds1)
                    | cs1 == cs = n
                    | otherwise = run' (n + 1) (cs1, ds1)

day23 :: IO ()
day23 = do
    elves <- fmap parseLines $ slurpLines "day23.txt" -- _sample
    let ds = [North, South, West, East]
    let t10 = foldl (\ a b -> step a) (elves, ds) [1..10]
    let r1 = (area (fst t10)) - (Set.size (fst t10))
    putStrLn ("Day 23, part 1: " ++ (show r1))
    let r2 = run (elves, ds)
    putStrLn ("Day 23, part 2: " ++ (show r2))

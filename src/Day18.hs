module Day18
    ( day18
    ) where

import qualified Data.List as List
import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Cube = (Int, Int, Int)

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> [Cube]
parseLines css = map parse css
    where
        parse :: String -> Cube
        parse cs = parse' $ map (\ a -> read a :: Int) $ splitOn "," cs
        parse' :: [Int] -> Cube
        parse' (x : y : z : _) = (x, y, z)

deltas :: [Cube]
deltas = [(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)]

countFaces :: (Cube -> Int) -> [Cube] -> Int
countFaces f xs = countFaces' 0 xs
    where
        countFaces' :: Int -> [Cube] -> Int
        countFaces' n [] = n
        countFaces' n ((x, y, z) : cs) = countFaces' (n + (foldl countFaces'' 0 deltas)) cs
            where
                countFaces'' :: Int -> Cube -> Int
                countFaces'' a (dx, dy, dz)
                    | (x + dx, y + dy, z + dz) `elem` xs = a
                    | otherwise = a + (f (x + dx, y + dy, z + dz))

exposed :: [Cube] -> Cube -> Int
exposed air c
    | c `elem`air = 1
    | otherwise = 0

flood :: [Cube] -> [Cube]
flood xs = flood' [] ((fr ++ fl ++ ft ++ fb ++ fa ++ fz) List.\\ xs)
    where
        maxX = maximum (map (\ (x, _, _) -> x) xs)
        minX = minimum (map (\ (x, _, _) -> x) xs)
        maxY = maximum (map (\ (_, y, _) -> y) xs)
        minY = minimum (map (\ (_, y, _) -> y) xs)
        maxZ = maximum (map (\ (_, _, z) -> z) xs)
        minZ = minimum (map (\ (_, _, z) -> z) xs)
        fr = [(maxX + 1, y, z) | y <- [minY..maxY], z <- [minZ..maxZ]]
        fl = [(minX - 1, y, z) | y <- [minY..maxY], z <- [minZ..maxZ]]
        ft = [(x, maxY + 1, z) | x <- [minX..maxX], z <- [minZ..maxZ]]
        fb = [(x, minY - 1, z) | x <- [minX..maxX], z <- [minZ..maxZ]]
        fa = [(x, y, maxZ + 1) | x <- [minX..maxX], y <- [minY..maxY]]
        fz = [(x, y, minZ - 1) | x <- [minX..maxX], y <- [minY..maxY]]
        flood' :: [Cube] -> [Cube] -> [Cube]
        flood' acc [] = acc
        flood' acc air = flood' (List.nub (acc ++ air)) (prune (List.nub $ concatMap explode air))
            where
                explode :: Cube -> [Cube]
                explode (x, y, z) = (foldl (\ a (dx, dy, dz) -> (x + dx, y + dy, z + dz) : a) [] deltas) List.\\ xs
                prune :: [Cube] -> [Cube]
                prune ys = (prune' (ys List.\\ xs)) List.\\ acc
        prune' :: [Cube] -> [Cube]
        prune' qs = filter (\ (x, y, z) -> (x >= minX) && (x <= maxX) && (y >= minY) && (y <= maxY) && (z >= minZ) && (z <= maxZ)) qs

day18 :: IO ()
day18 = do
    xs <- fmap parseLines $ slurpLines "day18.txt"
    let r1 = countFaces (\ _ -> 1) xs
    putStrLn ("Day 18, part 1: " ++ (show r1))
    let air = flood xs
    let r2 = countFaces (\ x -> exposed air x) xs
    putStrLn ("Day 18, part 2: " ++ (show r2))

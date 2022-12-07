module Day7
    ( day7
    ) where

import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Path = [String]

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> Map.Map Path Int
parseLines = parseLines' [] Map.empty
    where
        parseLines' :: Path -> Map.Map Path Int -> [String] -> Map.Map Path Int
        parseLines' _ m [] = m
        parseLines' pwd m (x : xs) = parseLines'' $ splitOn " " x
            where
                -- pwd is current path (working directory)
                -- m is a map from path to the sum of sizes of files in path any subpath of path
                parseLines'' :: [String] -> Map.Map Path Int
                parseLines'' ["$", "ls"] = parseLines' pwd m xs
                parseLines'' ["$", "cd", "/"] = parseLines' ["/"] m xs
                parseLines'' ["$", "cd", ".."] = parseLines' (tail pwd) m xs
                parseLines'' ["$", "cd", y] = parseLines' (y : pwd) m xs
                parseLines'' ["dir", _] = parseLines' pwd m xs
                parseLines'' [a, _] = parseLines' pwd (addSize (read a :: Int)) xs
                parseLines'' _ = error "bad input"
                addSize :: Int -> Map.Map Path Int
                -- add size for each path/subpath contained in pwd
                addSize sz = foldl (\ b a -> Map.insertWith (+) a sz b) m (pathsOf [] pwd)
                pathsOf :: [Path] -> Path -> [Path]
                -- each path/subpath contained in pwd
                pathsOf ps [] = ps
                pathsOf ps p = pathsOf (p : ps) (tail p)

freeSpace :: Map.Map Path Int -> Int
freeSpace m = 70000000 - m Map.! ["/"]

leastFit :: Map.Map Path Int -> Int -> Int
leastFit m needed = snd $ head $ sortOn (\ a -> snd a) (Map.toList $ Map.filter (\ a -> a >= needed) m)

day7 :: IO ()
day7 = do
    m <- fmap parseLines $ slurpLines "day7.txt"
    let r1 = Map.foldl (+) 0 (Map.filter (\ a -> a <= 100000) m)
    putStrLn ("Day 7, part 1: " ++ (show r1))
    let r2 = leastFit m (30000000 - freeSpace m)
    putStrLn ("Day 7, part 2: " ++ (show r2))

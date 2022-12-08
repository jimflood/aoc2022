module Day8
    ( day8
    ) where

import qualified Data.Map as Map
import Data.Tuple (swap)

type XYCoordinate = (Int, Int)

type Height = Int

type HeightMap = Map.Map XYCoordinate Height

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> HeightMap
parseLines xs = Map.fromList $ concatMap parseRow $ zipWith (\ y cs -> (y, cs)) [0..] xs
    where
        parseRow :: (Int, [Char]) -> [((Int, Int), Int)]
        parseRow (y, cs) = zipWith (\x c -> ((x, y), read [c] :: Int)) [0..] cs

leftToRight :: Int -> Int -> [XYCoordinate]
leftToRight maxX maxY = do
    y <- [0..maxY]
    x <- [0..maxX]
    return (x, y)

rightToLeft :: Int -> Int -> [XYCoordinate]
rightToLeft maxX maxY = reverse (leftToRight maxX maxY)

topToBottom :: Int -> Int -> [XYCoordinate]
topToBottom maxX maxY = map swap (leftToRight maxX maxY)

bottomToTop :: Int -> Int -> [XYCoordinate]
bottomToTop maxX maxY = reverse (topToBottom maxX maxY)

sight :: HeightMap -> [XYCoordinate] -> Map.Map XYCoordinate Bool
sight hm xys = Map.fromList $ sight' [] (-1) ((-1, -1) : xys)
    where
        sight' :: [(XYCoordinate, Bool)] -> Height -> [XYCoordinate] -> [(XYCoordinate, Bool)]
        sight' acc h ((x0, y0) : (x, y) : zs)
            | h > (-1) && ((x0 /= x) && (y0 /= y)) = sight' acc (-1) ((x0, y0) : (x, y) : zs)
            | otherwise = sight'' $ hm Map.! (x, y)
            where
                sight'' :: Int -> [(XYCoordinate, Bool)]
                sight'' hh
                    | hh > h = sight' (((x, y), True) : acc) hh ((x, y) : zs)
                    | otherwise = sight' (((x, y), False) : acc) h ((x, y) : zs)
        sight' acc _ _ = acc
 
scenicScore :: HeightMap -> Int -> Int -> XYCoordinate -> Int
scenicScore hm maxX maxY (x, y) = scoreLeft * scoreUp * scoreRight * scoreDown
    where
        scoreLeft :: Int
        scoreLeft = score 0 (-1, 0) (x, y)
        scoreUp :: Int
        scoreUp =  score 0 (0, -1) (x, y)
        scoreRight :: Int
        scoreRight =  score 0 (1, 0) (x, y)
        scoreDown :: Int
        scoreDown =  score 0 (0, 1) (x, y)
        score :: Int -> XYCoordinate -> XYCoordinate -> Int
        score acc (dx, dy) (ax, ay)
            | ((ax + dx) < 0) || ((ax + dx) > maxX) = acc
            | ((ay + dy) < 0) || ((ay + dy) > maxY) = acc
            | (hm Map.! (ax + dx, ay + dy)) >= (hm Map.! (x, y)) = acc + 1
            | otherwise = score (acc + 1) (dx, dy) (ax + dx, ay + dy)

day8 :: IO ()
day8 = do
    hm <- parseLines <$> slurpLines "day8.txt"
    let maxX = maximum $ map fst $ Map.keys hm
    let maxY = maximum $ map snd $ Map.keys hm
    let vms = map (\ f -> sight hm (f maxX maxY)) [leftToRight, rightToLeft, topToBottom, bottomToTop]
    let vm = Map.fromList $ map (\ k -> (k, any (\m -> m Map.! k) vms)) (Map.keys hm)
    let r1 = length (filter id $ map snd (Map.toList vm))
    putStrLn ("Day 8, part 1: " ++ (show r1))
    let r2 = maximum $ map (\ k -> scenicScore hm maxX maxY k) $ Map.keys hm
    putStrLn ("Day 8, part 2: " ++ (show r2))
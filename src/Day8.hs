module Day8
    ( day8
    ) where

import qualified Data.Map as Map
import Data.Tuple (swap)

type XYCoordinate = (Int, Int)

type Height = Int

type Bounds = (Int, Int)

type HeightMap = Map.Map XYCoordinate Height

type VisibilityMap = Map.Map XYCoordinate Bool

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> HeightMap
parseLines xs = Map.fromList $ concatMap parseRow $ zipWith (\ y cs -> (y, cs)) [0..] xs
    where
        parseRow :: (Int, [Char]) -> [((Int, Int), Int)]
        parseRow (y, cs) = zipWith (\x c -> ((x, y), read [c] :: Int)) [0..] cs

bounds :: HeightMap -> Bounds
bounds hm = (bounds' fst, bounds' snd)
    where
        bounds' :: ((Int, Int) -> Int) -> Int
        bounds' f = maximum $ map f $ Map.keys hm

sight :: HeightMap -> [XYCoordinate] -> VisibilityMap
-- dummy value (-1, -1) prepended only to satisfy the pattern match ((x0, y0): (x, y)) at the start
sight hm xys = Map.fromList $ sight' [] (-1) ((-1, -1) : xys)
    where
        sight' :: [(XYCoordinate, Bool)] -> Height -> [XYCoordinate] -> [(XYCoordinate, Bool)]
        sight' acc h ((x0, y0) : (x, y) : zs)
            -- this just resets the height to -1 at the start of each sightline (row or column)
            | h > (-1) && ((x0 /= x) && (y0 /= y)) = sight' acc (-1) ((x0, y0) : (x, y) : zs)
            | otherwise = sight'' $ hm Map.! (x, y)
            where
                sight'' :: Int -> [(XYCoordinate, Bool)]
                sight'' hh
                    -- simple check is the current tree height high enough to see?
                    | hh > h = sight' (((x, y), True) : acc) hh ((x, y) : zs)
                    | otherwise = sight' (((x, y), False) : acc) h ((x, y) : zs)
        sight' acc _ _ = acc

visible :: HeightMap -> Bounds -> VisibilityMap
-- consider the four directional sightlines to see if a tree is visible or not
visible hm (mx, my) = Map.fromList $ map (\ xy -> (xy, any (\m -> m Map.! xy) sightLines)) $ Map.keys hm
    where
        sightLines :: [VisibilityMap]
        -- these coordinates carefully ordered so that the simple sight algorithm works
        sightLines = map (\ xys -> sight hm xys) [leftToRight, rightToLeft, topToBottom, bottomToTop]
        leftToRight :: [XYCoordinate]
        leftToRight = do
            y <- [0..mx]
            x <- [0..my]
            return (x, y)
        rightToLeft ::[XYCoordinate]
        rightToLeft = reverse leftToRight
        topToBottom :: [XYCoordinate]
        topToBottom = map swap leftToRight
        bottomToTop :: [XYCoordinate]
        bottomToTop = reverse topToBottom

scenicScore :: HeightMap -> Bounds -> XYCoordinate -> Int
 -- a simple "walk towards each edge" search
scenicScore hm (mx, my) (x, y) = scoreLeft * scoreUp * scoreRight * scoreDown
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
            | ((ax + dx) < 0) || ((ax + dx) > mx) = acc
            | ((ay + dy) < 0) || ((ay + dy) > my) = acc
            | (hm Map.! (ax + dx, ay + dy)) >= (hm Map.! (x, y)) = acc + 1
            | otherwise = score (acc + 1) (dx, dy) (ax + dx, ay + dy)

day8 :: IO ()
day8 = do
    hm <- parseLines <$> slurpLines "day8.txt"
    let bs = bounds hm
    let vm = visible hm bs
    let r1 = length (filter id $ map snd (Map.toList vm))
    putStrLn ("Day 8, part 1: " ++ (show r1))
    let r2 = maximum $ map (\ k -> scenicScore hm bs k) $ Map.keys hm
    putStrLn ("Day 8, part 2: " ++ (show r2))

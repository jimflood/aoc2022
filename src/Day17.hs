module Day17
    ( day17
    ) where

import Data.List
import qualified Data.Map as Map

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

type Coordinate = (Int, Int)

type Rock = [Coordinate]

type Chamber = Map.Map Coordinate Int

type HeightMap = Map.Map Int Int

type State = ([Rock], [Char], HeightMap, Chamber)

-- ####
rock1 :: Rock
rock1 = [(0, 0), (1, 0), (2, 0), (3, 0)]

-- .#.
-- ###
-- .#.
rock2 :: Rock
rock2 = [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]

-- ..#
-- ..#
-- ###
rock3 :: Rock
rock3 = [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]

-- #
-- #
-- #
-- #
rock4 :: Rock
rock4 = [(0, 0), (0, 1), (0, 2), (0, 3)]

-- ##
-- ##
rock5 :: Rock
rock5 = [(0, 0), (1, 0), (0, 1), (1, 1)]

translate :: Rock -> Coordinate -> Rock
translate s (dx, dy) = map (\ (x, y) -> (x + dx, y + dy)) s

dropPt :: Chamber -> Coordinate
dropPt ch
    | Map.size ch == 0 = (2, 3)
    | otherwise = (2, maximum (map snd (Map.keys ch)) + 4)

startRock :: Chamber -> Rock -> Rock
startRock ch r = translate r (dropPt ch)

run :: Int -> State -> State
run numRocks = run' 1
    where
        run' :: Int -> State -> State
        run' count s
            | count > numRocks = s
            | otherwise = run' (count + 1) (nextRock count s)

nextRock :: Int -> State -> State
nextRock rid ((r : rs), js, hm, ch) = dropRock rid (startRock ch r) (rs, js, hm, ch)

dropRock :: Int -> Rock -> State -> State
dropRock rid r (rs, (j : js), hm, ch) = lowerRock rid (blowRock ch r j) (rs, js, hm, ch)

blowRock :: Chamber -> Rock -> Char -> Rock
blowRock ch r j
    | (j == '<') && ((minimum (map fst r)) > 0) = blowRock' $ translate r ((-1), 0)
    | (j == '>') && ((maximum (map fst r)) < 6) = blowRock' $ translate r (1, 0)
    | otherwise = r
    where
        blowRock' :: Rock -> Rock
        blowRock' nr
            | null (intersect (Map.keys ch) nr) = nr
            | otherwise = r

lowerRock :: Int -> Rock -> State -> State
lowerRock rid r (rs, js, hm, ch) = lowerRock' $ translate r (0, (-1))
    where
        lowerRock' :: Rock -> State
        lowerRock' nr
            | (minimum (map snd nr) > -1) && (null (intersect (Map.keys ch) nr)) = dropRock rid nr (rs, js, hm, ch)
            | otherwise = lowerRock'' (addRock rid ch r) -- add previous
        lowerRock'' :: Chamber -> State
        lowerRock'' nch = lowerRock''' (heightOf nch)
            where
                lowerRock''' h = (rs, js, Map.insert rid h hm, prune h nch)

addRock :: Int -> Chamber -> Rock -> Chamber
addRock rid ch r = foldl (\ a b -> Map.insert b rid a) ch r

-- prune bottom of chamber for speed (50 found by trial and error)
prune :: Int -> Chamber -> Chamber
prune h ch = Map.fromList $ filter (\ ((_, y), _) -> y > (h - 50)) $ Map.toList ch

heightOf :: Chamber -> Int
heightOf ch = 1 + maximum (map snd (Map.keys ch))

align :: [Int] -> Int
align xs = align' ((length xs) `div` 2)
    where
        align' :: Int -> Int
        align' n = align'' 5 (drop n xs) (drop (n + 5) xs)
        align'' :: Int -> [Int] -> [Int] -> Int
        align'' _ _ [] = error "exhausted"
        align'' offset as bs = align''' (min (length as) (length bs))
            where
                align''' :: Int -> Int
                align''' c
                    | (take c as) == (take c bs) = offset
                    | otherwise = align'' (offset + 5) as (drop 5 bs)

-- for each rock the relative increase in overall height
diffs :: HeightMap -> [Int]
diffs hm = diffs' $ map snd $ Map.toAscList hm
    where
        diffs' :: [Int] -> [Int]
        diffs' hs = map (\ (a, b) -> b - a) $ zip (0 : hs) hs

day17 :: IO ()
day17 = do
    p <- fmap head $ slurpLines "day17.txt" -- _sample
    let jets = cycle p
    let rocks = cycle [rock1, rock2, rock3, rock4, rock5]
    let (_, _, hm1, _) = run 4000 (rocks, jets, Map.empty, Map.empty)
    let r1 = hm1 Map.! 2022
    putStrLn ("Day 17, part 1: " ++ (show r1))
    let fq = align $ diffs hm1
    let a = 1000000000000 `div` fq
    let b = 1000000000000 `mod` fq
    let h1 = hm1 Map.! fq
    let h2 = hm1 Map.! (fq * 2)
    let h3 = hm1 Map.! (fq + b)
    let r2 = h1 + ((a - 1) * (h2 - h1)) + (h3 - h1)
    putStrLn ("Day 17, part 2: " ++ (show r2))
 
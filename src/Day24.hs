module Day24
    ( day24
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List (sortOn)

type Coordinate = (Int, Int)

type CoordinateSet = Set.Set Coordinate

type Blizzard = (Coordinate, Char)

type BlizzardSet = Set.Set Blizzard

data FloorTile = Wall | Ground
    deriving (Eq, Show)

type ValleyMap = Map.Map Coordinate FloorTile

type Minute = Int

type Step = (Coordinate, Minute)

type StepSet = Set.Set Step

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseGrid :: [String] -> [(Coordinate, Char)]
parseGrid css = concatMap (\ (y, cs) -> map (\ (x, c) -> ((x, y), c)) (zip [0..] cs)) $ zip [0..] css

parseLines :: [String] -> (ValleyMap, BlizzardSet)
parseLines css = parseLines' $ parseGrid css
    where
        parseLines' :: [(Coordinate, Char)] -> (ValleyMap, BlizzardSet)
        parseLines' xycs = (valley, blizzards)
            where
                valley :: ValleyMap
                valley = Map.fromList $ map valley' xycs
                valley' :: (Coordinate, Char) -> (Coordinate, FloorTile)
                valley' (xy, '#') = (xy, Wall)
                valley' (xy, _) = (xy, Ground)
                blizzards :: BlizzardSet
                blizzards = Set.fromList $ mapMaybe blizzards' xycs
                blizzards' :: (Coordinate, Char) -> Maybe Blizzard
                blizzards' (xy, c)
                    | c `elem` ['^', 'v', '<', '>'] = Just (xy, c)
                    | otherwise = Nothing

{-
measure :: (Coordinate -> Int) -> (Map.Map Coordinate a) -> Int
measure f m = maximum (map f (Map.keys m))

display :: (ValleyMap, BlizzardSet) -> IO ()
display (vm, bm) = display' (measure fst vm) (measure snd vm) (Set.toList bm)
    where
        display' :: Int -> Int -> [Blizzard] -> IO ()
        display' a b storms = display'' [0..a] [0..b]
            where
                display'' :: [Int] -> [Int] -> IO ()
                display'' _ [] = return ()
                display'' [] (y : ys) = do
                    putStrLn ""
                    display'' [0..a] ys
                display'' (x : xs) (y : ys) = do
                    putStr $ draw (x, y)
                    display'' xs (y : ys)
                draw :: Coordinate -> String
                draw (x, y) = draw' $ vm Map.! (x, y)
                    where
                        draw' :: FloorTile -> String
                        draw' Wall = "#"
                        draw' _ = draw'' weather
                        draw'' :: [Char] -> String
                        draw'' [] = "."
                        draw'' [c] = [c]
                        draw'' cs = show (length cs)
                        weather :: [Char]
                        weather = map snd $ filter (\ ((a, b), c) -> (a, b) == (x, y)) storms
-}

step :: (ValleyMap, BlizzardSet) -> (ValleyMap, BlizzardSet)
step (vm, bs) = (vm, Set.map step' bs)
    where
        g :: [Coordinate]
        g = ground vm
        step' :: Blizzard -> Blizzard
        step' ((x, y), c) = (wrap (x + dx, y + dy), c)
            where
                dx :: Int
                dx
                    | c == '>' = 1
                    | c == '<' = -1
                    | otherwise = 0
                dy :: Int
                dy
                    | c == 'v' = 1
                    | c == '^' = -1
                    | otherwise = 0
                wrap :: Coordinate -> Coordinate
                wrap (a, b)
                    | (a, b) `elem` g = (a, b)
                    | otherwise = wrap' c
                        where
                            wrap' :: Char -> Coordinate
                            wrap' '^' = (a, maximum $ map snd $ filter ((==a) . fst) g)
                            wrap' 'v' = (a, minimum $ map snd $ filter ((==a) . fst) g)
                            wrap' '<' = (maximum $ map fst $ filter ((==b) . snd) g, b)
                            wrap' '>' = (minimum $ map fst $ filter ((==b) . snd) g, b)
                            wrap' _ = error "cannot occur"

ground :: ValleyMap -> [Coordinate]
ground vm = Map.keys $ Map.filter (==Ground) vm

findStart :: ValleyMap -> Coordinate
findStart vm = head $ sortOn snd (ground vm)

findEnd :: ValleyMap -> Coordinate
findEnd vm = head $ reverse $ sortOn snd (ground vm)

solve :: ValleyMap -> BlizzardSet -> [Coordinate] -> Minute
solve vm bs0 waypoints = solve' 1 (Set.singleton (head waypoints, 0)) (snd (step (vm, bs0))) (tail waypoints)
    where
        gs :: CoordinateSet
        gs = Set.fromList $ ground vm
        solve' :: Minute -> StepSet -> BlizzardSet -> [Coordinate] -> Minute
        solve' n steps bs (w : ws) = solve'' $ Set.foldl findSunshine Set.empty stepCloud
            where
                findSunshine :: CoordinateSet -> Coordinate -> CoordinateSet
                findSunshine a b = Set.union a $ Set.intersection (candidates b) sunny
                sunny :: CoordinateSet
                sunny = Set.difference gs (Set.map fst bs)
                stepCloud :: CoordinateSet
                stepCloud = Set.map fst $ Set.filter (\ (c, m) -> m == (n - 1)) steps
                candidates :: Coordinate -> CoordinateSet
                candidates (x, y) = Set.fromList [(x, y), (x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
                solve'' :: CoordinateSet -> Minute
                solve'' sunshine
                    | (Set.member w sunshine) && null ws = n -- solved
                    | (Set.member w sunshine) = solve' (n + 1) (Set.singleton (w, n)) nextBlizzard ws
                    | otherwise = solve' (n + 1) nextSteps nextBlizzard (w : ws)
                    where
                        nextBlizzard :: BlizzardSet
                        nextBlizzard = snd (step (vm, bs))
                        nextSteps = Set.union steps $ Set.map (\ a -> (a, n)) sunshine

day24 :: IO ()
day24 = do
    (vm, bs) <- fmap parseLines $ slurpLines "day24.txt"
    let start = findStart vm
    let end = findEnd vm
    let r1 = solve vm bs [start, end]
    putStrLn ("Day 24, part 1: " ++ (show r1))
    let r2 = solve vm bs [start, end, start, end]
    putStrLn ("Day 24, part 2: " ++ (show r2))

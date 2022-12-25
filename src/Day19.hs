module Day19
    ( day19
    ) where

import Data.List
import Data.List.Split (splitOneOf)
import Data.Maybe
import qualified Data.Map as Map

data Material = Ore | Clay | Obsidian | Geode
    deriving (Eq, Ord, Show)

data Robot = Robot {
    collects :: Material,
    consumes :: [(Int, Material)]
} deriving (Eq, Show)

data Recipe = Recipe {
    robot :: Material,
    cost :: [(Int, Material)]
} deriving (Eq, Show)

data Blueprint = Blueprint {
    number :: Int,
    recipes :: [Recipe]
} deriving (Eq, Show)

type MaterialMap = Map.Map Material Int

data Scenario = Scenario {
    blueprint :: Blueprint,
    minutes :: Int,
    inventory :: MaterialMap,
    robots :: MaterialMap,
    planned :: Maybe Material,
    history :: [Scenario]
} deriving (Show)

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

toInt :: String -> Int
toInt a = read a :: Int

toMat :: String -> Material
toMat "ore" = Ore
toMat "clay" = Clay
toMat "obsidian" = Obsidian
toMat "geode" = Geode
toMat _ = error "bad input"

parseLines :: [String] -> [Blueprint]
parseLines xs = map parse xs
    where
        parse :: String -> Blueprint
        parse = parse' . (splitOneOf " :.")
        parse' :: [String] -> Blueprint
        parse' ("Blueprint" : a : "" : css) = parse'' (toInt a) css
        parse'' :: Int -> [String] -> Blueprint
        parse'' n = parse''' []
            where
                parse''' :: [Recipe] -> [String] -> Blueprint
                parse''' rs [] = Blueprint n rs
                parse''' rs ("Each" : m : "robot" : "costs" : ys) = parse'''' [] ys
                    where
                        parse'''' :: [(Int, Material)] -> [String] -> Blueprint
                        parse'''' acc ("" : zs) = parse''' ((Recipe (toMat m) acc) : rs) zs
                        parse'''' acc ("and" : zs) = parse'''' acc zs
                        parse'''' acc (a : b : zs) = parse'''' ((toInt a, toMat b) : acc) zs

step :: Scenario -> Int -> [Scenario]
step sc minute = map collect (build sc)
    where
        -- note the collect uses previous robots (before spending)
        collect :: Scenario -> Scenario
        collect nsc = Scenario (blueprint nsc) minute (collect' sc) (robots nsc) (planned nsc) (sc : history nsc)
            where
                collect' :: Scenario -> MaterialMap
                collect' xsc = foldl collect'' (inventory nsc) (Map.toList (robots xsc))
                collect'' :: MaterialMap -> (Material, Int) -> MaterialMap
                collect'' im (mat, n) = Map.insertWith (+) mat n im
 
build :: Scenario -> [Scenario]
build sc = map fulfill (plan sc)

plan :: Scenario -> [Scenario]
plan s
    | isJust (planned s) = [s]
    | otherwise = plan' $ foldl capableOf [] (recipes (blueprint s))
    where
        capableOf :: [Material] -> Recipe -> [Material]
        capableOf a (Recipe mat cs)
            | null $ (map snd cs) \\ producing = mat : a
            | otherwise = a
        producing :: [Material]
        producing = map fst $ filter (\ (mat, c) -> c > 0) $ Map.toList (robots s)
        plan' :: [Material] -> [Scenario]        
        plan' mats = map (\ m -> Scenario (blueprint s) (minutes s) (inventory s) (robots s) (Just m) (history s)) mats

fulfill :: Scenario -> Scenario
fulfill s = fulfill' $ planned s
    where
        fulfill' :: Maybe Material -> Scenario
        fulfill' (Just r) = fulfill'' $ fromJust $ find (\ a -> (robot a == r)) (recipes (blueprint s))
            where
                fulfill'' :: Recipe -> Scenario
                fulfill'' (Recipe _ cs)
                    | any (==True) (map (\ (n, mat) -> ((inventory s) Map.! mat) < n) cs) = s -- insufficient inventory
                    | otherwise = addRobot
                    where
                        addRobot :: Scenario    
                        addRobot = Scenario (blueprint s) (minutes s) buyParts (incrementMat r (robots s)) Nothing (history s)
                        buyParts :: MaterialMap
                        buyParts = foldl (\ a (nn, mmat) -> subtractMat nn mmat a) (inventory s) cs

prunex :: [Scenario] -> [Scenario]
prunex xs
    | any (\ x -> ((robots x) Map.! Geode) > 0) xs = prunex'
    | otherwise = xs
    where
        prunex' = filter (\ x -> ((robots x) Map.! Obsidian) > 0) xs

prune1 :: [Scenario] -> [Scenario]
-- 50000 empirically estimated
prune1 xs = take 50000 $ reverse $ sortOn (\x -> weight x) (prunex xs)
    where
        weight s = (((robots s) Map.! Geode) * 1000000) + (((inventory s) Map.! Geode) * 100000) + (((robots s) Map.! Obsidian) * 100000) + (((inventory s) Map.! Obsidian) * 10000) + (((robots s) Map.! Clay) * 1000) + (((inventory s) Map.! Clay) * 100) + (((robots s) Map.! Ore) * 10) + (((inventory s) Map.! Ore))

subtractMat :: Int -> Material -> MaterialMap -> MaterialMap
subtractMat count mat mm = Map.insertWith (\ a b -> b - a) mat count mm

incrementMat :: Material -> MaterialMap -> MaterialMap
incrementMat mat mm = Map.insertWith (+) mat 1 mm
 
emptyMatMap :: MaterialMap
emptyMatMap = Map.fromList [(Ore, 0), (Clay, 0), (Obsidian, 0), (Geode, 0)]

{-
display [] = return ()
display (x : xs) = do
    print ("------------------------------------------------------------------------")
    print ("minutes:   " ++ show (minutes x))
    print ("robots:    " ++ show (robots x))
    print ("inventory: " ++ show (inventory x))
    print ("planned:   " ++ show (planned x))
    display xs
-}

run :: Int -> Blueprint -> Scenario
run minutes b = do
    let start = Scenario b 0 emptyMatMap (incrementMat Ore emptyMatMap) Nothing []
    let r = foldl (\ a i -> prune1 (concatMap (\ x -> step x i) a)) [start] [1..minutes]
    let mg = maximum (map (\ x -> (inventory x) Map.! Geode) r)
    fromJust $ find (\ x -> ((inventory x) Map.! Geode) == mg) r

solve :: Int -> ([(Int, Int)] -> Int) -> [Blueprint] -> Int
solve minutes scorer bs = scorer $ map (\ b -> (number b, (inventory (run minutes b)) Map.! Geode)) bs

day19 :: IO ()
day19 = do
    bs <- fmap parseLines $ slurpLines "day19.txt" -- _sample
    let r1 = solve 24 (\ xs -> sum $ map (\ (a, b) -> (a * b)) xs) bs
    putStrLn ("Day 19, part 1: " ++ (show r1))
    let r2 = solve 32 (\ xs -> product $ map (\ (a, b) -> b) xs) (take 3 bs)
    putStrLn ("Day 19, part 2: " ++ (show r2))

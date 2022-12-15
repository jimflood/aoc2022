module Day14
    ( day14
    ) where

import Data.List (intercalate)
import Data.List.Split (splitOneOf)
import qualified Data.Map as Map

data Material = Rock | Sand | Air | Source
    deriving (Eq, Show)

type Coordinate = (Int, Int)

type Scan = Map.Map Coordinate Material

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> Scan
parseLines xs = Map.fromList $ map (\ c -> (c, Rock)) $ concatMap parseLines' xs
    where
        parseLines' :: String -> [Coordinate]
        parseLines' cs = points [] $ filter (not . null) (splitOneOf ", ->" cs)
        points :: [Coordinate] -> [String] -> [Coordinate]
        points acc [] = concat $ segments [] acc
        points acc (a : b : cs) = points ((read a :: Int, read b :: Int) : acc) cs
        segments :: [[Coordinate]] -> [Coordinate] -> [[Coordinate]]
        segments acc [_] = acc
        segments acc (a : b : cs)
            | fst a == fst b = segments ((map (\ y -> (fst a, y)) [(min (snd a) (snd b))..(max (snd a) (snd b))]) : acc) (b : cs)
            | otherwise = segments ((map (\ x -> (x, snd a)) [(min (fst a) (fst b))..(max (fst a) (fst b))]) : acc) (b : cs)
        segments _ _ = error "fault"

display :: Scan -> String
display m = intercalate "\n" $ chop [] width $ map spriteOf materials
    where
        materials = do
            y <- [(minimum ycoords)..(maximum ycoords)]
            x <- [(minimum xcoords)..(maximum xcoords)]
            return (Map.findWithDefault Air (x, y) m)
        xcoords :: [Int]
        xcoords = map (fst . fst) $ Map.toList m
        ycoords :: [Int]
        ycoords = map (snd . fst) $ Map.toList m
        width = 1 + (maximum xcoords) - (minimum xcoords)
        spriteOf :: Material -> Char
        spriteOf Rock = '#'
        spriteOf Sand = 'o'
        spriteOf Air = '.'
        spriteOf Source = '+'
        chop :: [String] -> Int -> String -> [String]
        chop acc _ [] = reverse acc
        chop acc n cs = chop ((take n cs) : acc) n (drop n cs)

findByMaterial :: Scan -> Material -> [Coordinate]
findByMaterial m mat = map fst $ filter (\ (_, a) -> a == mat) $ Map.toList m

bottom :: Scan -> Int
bottom m = maximum $ map snd $ Map.keys m

run :: Bool -> Coordinate -> Scan -> Scan
run hasFloor source m0 = run' (bottom m0) source m0
    where
        run' :: Int -> Coordinate -> Scan -> Scan
        run' maxy = run''
            where
                run'' :: Coordinate -> Scan -> Scan
                run'' (x, y) m
                    | hasFloor && y == maxy + 1 = run'' source (Map.insert (x, y) Sand m)
                    | y == maxy + 1 = m
                    | Map.notMember (x, y + 1) m = run'' (x, y + 1) m
                    | Map.notMember (x - 1, y + 1) m = run'' (x - 1, y + 1) m
                    | Map.notMember (x + 1, y + 1) m = run'' (x + 1, y + 1) m
                    | x == fst source && y == snd source = (Map.insert (x, y) Sand m)
                    | otherwise = run'' source (Map.insert (x, y) Sand m)

day14 :: IO ()
day14 = do
    m <- fmap parseLines $ slurpLines "day14.txt"
    let m0 = Map.insert (500, 0) Source m
    let m1 = run False (500, 0) m0
    let r1 = length $ findByMaterial m1 Sand
    putStrLn ("Day 14, part 1: " ++ (show r1))
    let m2 = run True (500, 0) m0
    let r2 = length $ findByMaterial m2 Sand
    putStrLn ("Day 14, part 2: " ++ (show r2))

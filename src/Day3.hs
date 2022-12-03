module Day3
    ( day3
    ) where

import Data.List
import Data.Char (ord)
import Data.Maybe

type Compartment = [Char]

type Rucksack = (Compartment, Compartment)

type Badge = Char

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> [Rucksack]
parseLines = map parseLines'
    where
        parseLines' :: String -> Rucksack
        parseLines' = parseLines'' [] []
        parseLines'' :: Compartment -> Compartment -> String -> Rucksack
        parseLines'' xs ys [] = (reverse xs, ys)
        parseLines'' xs ys (c : cs) = parseLines'' (c : xs) ((last cs) : ys) (init cs)

commonItem :: Rucksack -> Char
commonItem r = commonItem' (fst r) (snd r)
    where
        commonItem' :: [Char] -> [Char] -> Char
        commonItem' xs ys = commonItem'' $ intersect (nub xs) (nub ys)
        commonItem'' [c] = c
        commonItem'' _ = error "Nope"

priority :: Char -> Int
priority c
    | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
    | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27
    | otherwise = error "Nope"

badges :: [Rucksack] -> [Badge]
badges = badges' []
    where
        badges' :: [Badge] -> [Rucksack] -> [Badge]
        badges' acc [] = acc
        badges' acc (a : b : c : ds) = badges' (common [a, b, c] : acc) ds
        badges' _ _ = error "Nope"
        common :: [Rucksack] -> Badge
        common xs = common' (map contents xs)
        common' :: [[Char]] -> Badge
        common' css = common'' $ foldl intersect (head css) (tail css)
        common'' :: [Char] -> Badge
        common'' [c] = c
        common'' _ = error "Nope"
        contents :: Rucksack -> [Char]
        contents r = nub $ fst r ++ snd r

day3 :: IO ()
day3 = do
    xs <- fmap parseLines $ slurpLines "day3.txt"
    let r1 = sum $ map (priority . commonItem) xs
    putStrLn ("Day 3, part 1: " ++ (show r1))
    let r2 = sum $ map priority (badges xs)
    putStrLn ("Day 3, part 2: " ++ (show r2))

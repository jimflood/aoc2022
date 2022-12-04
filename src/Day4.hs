module Day4
    ( day4
    ) where

import Data.List
import Data.List.Split (splitOneOf)

type SectionId = Int

type PairOfElves = ([SectionId], [SectionId])

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> [PairOfElves]
parseLines = map parseLines'
    where
        parseLines' :: String -> PairOfElves
        parseLines' s = parseLines'' $ map (\ x -> read x :: Int) $ splitOneOf "-," s
        parseLines'' :: [Int] -> PairOfElves
        parseLines'' [a, b, c, d] = ([a..b], [c..d])
        parseLines'' _ = error "Nope"

needsReconsideration :: PairOfElves -> Bool
needsReconsideration p
    | fst p \\ snd p == [] = True
    | snd p \\ fst p == [] = True
    | otherwise = False

overlapAtAll :: PairOfElves -> Bool
overlapAtAll p = fst p `intersect` snd p /= []

day4 :: IO ()
day4 = do
    xs <- fmap parseLines $ slurpLines "day4.txt"
    let r1 = length $ filter needsReconsideration xs
    putStrLn ("Day 4, part 1: " ++ (show r1))
    let r2 = length $ filter overlapAtAll xs
    putStrLn ("Day 4, part 2: " ++ (show r2))

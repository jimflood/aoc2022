module Day20
    ( day20
    ) where

import Data.List
import Data.Maybe (fromJust)

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> [Int]
parseLines = map (\ x -> read x :: Int)

type Value = Int

type MixOrder = Int

type Mixable = (MixOrder, Value)

mix :: [Mixable] -> Int -> Int -> Int -> [Mixable]
mix ms len i n
    | n == 0 = ms
    | otherwise = mix' ((drop i ms) ++ (take i ms))
        where
            mix' :: [Mixable] -> [Mixable]
            mix' xs
                | n > 0 = take n (tail xs) ++ take 1 xs ++ drop (n + 1) xs
                | otherwise = take (len + n - 1) (tail xs) ++ take 1 xs ++ drop (len + n - 1) (tail xs)

rotate :: Int -> [Mixable] -> MixOrder -> [Mixable]
rotate len ms mi = rotate' (findIndex (\ a -> fst a == mi) ms)
    where
        rotate' :: Maybe Int -> [Mixable]
        rotate' Nothing = error "nope"
        rotate' (Just i) = rotate'' ((snd (ms !! i)) `rem` (len - 1))
            where
                rotate'' :: Int -> [Mixable]
                rotate'' n = mix ms len i n

result :: [Mixable] -> Int
result ms = sum [valueAt 1000, valueAt 2000, valueAt 3000]
    where
        len = length ms
        zi = fromJust $ findIndex (\ a -> snd a == 0) ms
        valueAt :: Int -> Int
        valueAt n = snd $ ms !! ((zi + n) `rem` len)
 
day20 :: IO ()
day20 = do
    xs <- fmap parseLines $ slurpLines "day20.txt"
    let indexes = [0..(length xs - 1)]
    let mixables = zip indexes xs
    let len = length mixables
    let xs1 = foldl (rotate len) mixables indexes
    let r1 = result xs1
    putStrLn ("Day 20, part 1: " ++ (show r1))
    let mixables2 = zip indexes (map (\ x -> x * 811589153) xs)
    let xs2 = foldl (\ a _ -> (foldl (rotate len) a indexes)) mixables2 [1..10]
    let r2 = result xs2
    putStrLn ("Day 20, part 2: " ++ (show r2))


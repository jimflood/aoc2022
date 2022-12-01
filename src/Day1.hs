module Day1
    ( day1
    ) where

import Text.Read
import Data.List (sort)

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> [Maybe Integer]
parseLines xs = map (\ a -> readMaybe a :: Maybe Integer) xs

comb :: [Maybe Integer] -> [Integer]
comb xs = foldl comb' [] xs
    where
        comb' :: [Integer] -> Maybe Integer -> [Integer]
        comb' [] (Just x) = [x]
        comb' ys Nothing = (0 : ys)
        comb' (y : ys) (Just x) = (y + x : ys)

day1 :: IO ()
day1 = do
    xs <- fmap parseLines $ slurpLines "day1.txt"
    let ys = comb xs
    let r1 = maximum ys
    putStrLn ("Day 1, part 1: " ++ (show r1))
    let oys = take 3 $ reverse $ sort ys
    let r2 = sum oys
    putStrLn ("Day 1, part 2: " ++ (show r2))

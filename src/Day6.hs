module Day6
    ( day6
    ) where

import Data.List

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

scan :: Int -> String -> Int
scan markerLen = scan' 0 
    where
        scan' :: Int -> String -> Int
        scan' acc x
            | length (nub (take markerLen x)) == markerLen = acc + markerLen
            | otherwise = scan' (acc + 1) (tail x)

day6 :: IO ()
day6 = do
    buffer <- head <$> slurpLines "day6.txt"
    let r1 = scan 4 buffer
    putStrLn ("Day 6, part 1: " ++ (show r1))
    let r2 = scan 14 buffer
    putStrLn ("Day 6, part 2: " ++ (show r2))
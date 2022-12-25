module Day25
    ( day25
    ) where

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

decode :: String -> Int
decode s = decode' 0 s
    where
        decode' :: Int -> String -> Int
        decode' acc [] = acc
        decode' acc (c : cs) = decode' ((5 * acc) + (snafu c)) cs
            where
                snafu :: Char -> Int
                snafu '2' = 2
                snafu '1' = 1
                snafu '0' = 0
                snafu '-' = -1
                snafu '=' = -2
 
encode :: Int -> String
encode = ufans . squeeze . fives
    where
        squeeze :: [Int] -> [Int]
        squeeze = squeeze' []
            where
                squeeze' :: [Int] -> [Int] -> [Int]
                squeeze' acc [] = acc
                squeeze' acc (x : xs)
                    | x == 5 && null xs = squeeze' (0 : acc) [1]
                    | x == 4 && null xs = squeeze' ((-1) : acc) [1]
                    | x == 3 && null xs = squeeze' ((-2) : acc) [1]
                    | x == 5 = squeeze' (0 : acc) ((head xs) + 1 : tail xs)
                    | x == 4 = squeeze' ((-1) : acc) ((head xs) + 1 : tail xs)
                    | x == 3 = squeeze' ((-2) : acc) ((head xs) + 1 : tail xs)
                    | otherwise = squeeze' (x : acc) xs
        fives :: Int -> [Int]
        fives n = fives' [] (width n) n
            where
                fives' :: [Int] -> Int -> Int -> [Int]
                fives' acc w x
                    | w < 2 = ((x `rem` 5) : acc)
                    | otherwise = fives' (x `div` (5 ^ (w - 1)) : acc) (w - 1) (x `rem` (5 ^ (w - 1)))
        width :: Int -> Int
        width = width' 0
            where
                width' :: Int -> Int -> Int
                width' acc 0 = acc
                width' acc x = width' (acc + 1) (x `div` 5)
        ufans :: [Int] -> String
        ufans xs = map ufans' xs
            where
                ufans' 2 = '2'
                ufans' 1 = '1'
                ufans' 0 = '0'
                ufans' (-1) = '-'
                ufans' (-2) = '='
                ufans' _ = error "ruh roh"

day25 :: IO ()
day25 = do
    xs <- slurpLines "day25.txt"
    let r1 = encode $ sum $ map decode xs
    putStrLn ("Day 25, part 1: " ++ (show r1))

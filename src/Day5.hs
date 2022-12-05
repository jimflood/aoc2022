module Day5
    ( day5
    ) where

import Data.List.Split (splitOn)
import qualified Data.Map as Map

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

data Move = Move {
        move :: Int,
        from :: Int,
        to :: Int
    } deriving (Show)

type Crate = Char

type Dock = Map.Map Int [Crate]

--     [D]    
-- [N] [C]    
-- [Z] [M] [P]
--  1   2   3 

-- move 1 from 2 to 1
-- move 3 from 1 to 3
-- move 2 from 2 to 1
-- move 1 from 1 to 2

splitInput :: [String] -> ((String, [String]), [String])
splitInput = splitInput' []
    where
        splitInput' :: [String] -> [String] -> ((String, [String]), [String])
        splitInput' acc (x : y : zs)
            | '[' `notElem` x = ((x, acc), zs)
            | otherwise = splitInput' (x : acc) (y : zs)

toInt :: String -> Int
toInt s = read s :: Int

parseCrates :: (String, [String]) -> Dock
parseCrates (h, xs) = foldl parseCrates' Map.empty xs
    where
        parseCrates' :: Dock -> String -> Dock
        parseCrates' d [] = d
        parseCrates' d cs = parseCrates' (parseCrates'' d (keyOf cs) (take 4 cs)) (drop 4 cs)
        parseCrates'' :: Dock -> Int -> String -> Dock
        parseCrates'' d k (a : b : _ : _)
            | a == '[' = Map.insertWith (++) k [b] d
            | otherwise = d
        keyOf :: String -> Int
        keyOf s = (4 + length h - length s) `div` 4

parseMoves :: [String] -> [Move]
parseMoves = map (parseMoves' . (splitOn " "))
    where
        parseMoves' :: [String] -> Move
        parseMoves' ["move", a, "from", b, "to", c] = Move (toInt a) (toInt b) (toInt c)

parseLines :: [String] -> (Dock, [Move])
parseLines xs = parseLines' $ splitInput xs
    where
        parseLines' :: ((String, [String]), [String]) -> (Dock, [Move])
        parseLines' ts = (parseCrates (fst ts), parseMoves (snd ts))

transfer :: ([Crate] -> [Crate]) -> Dock -> [Move] -> Dock
transfer op d ms = foldl transfer' d ms
    where
        transfer' :: Dock -> Move -> Dock
        transfer' dd (Move n f t) = transfer'' (Map.lookup f dd)
            where
                transfer'' :: Maybe [Crate] -> Dock
                transfer'' (Just fs) = foldl transfer''' dd (op (take n fs))
                    where
                        transfer''' :: Dock -> Char -> Dock
                        transfer''' ddd c = Map.adjust (\ xs -> c : xs) t (Map.adjust (\ xs -> tail xs) f ddd)

topCrates :: Dock -> [Crate]
topCrates d = map (head . snd) (Map.toAscList d)

day5 :: IO ()
day5 = do
    (dock, moves) <- fmap parseLines $ slurpLines "day5.txt"
    let dock1 = transfer (\ xs -> xs) dock moves
    let r1 = topCrates dock1
    putStrLn ("Day 5, part 1: " ++ (show r1))
    let dock2 = transfer (\ xs -> reverse xs) dock moves
    let r2 = topCrates dock2
    putStrLn ("Day 5, part 2: " ++ (show r2))
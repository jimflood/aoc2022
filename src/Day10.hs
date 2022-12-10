module Day10
    ( day10
    ) where

import Data.List.Split (splitOn)

data Instruction = Noop | AddX Int
    deriving (Show)

type Cycle = Int

type Position = Int

type Pixel = String

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

split :: String -> [String]
split = splitOn " "

parseLines :: [String] -> [Instruction]
parseLines = map (parseLines' . split)
    where
        parseLines' :: [String] -> Instruction
        parseLines' ["noop"] = Noop
        parseLines' ["addx", a] = AddX (read a :: Int)
        parseLines' _ = error "bad instruction"

run :: [Instruction] -> [(Cycle, Int)]
run = run' [] [1..] 1 Nothing
    where
        run' :: [(Cycle, Int)] -> [Cycle] -> Int -> Maybe Instruction -> [Instruction] -> [(Cycle, Int)]
        run' acc (c : cs) x Nothing (Noop : ys) = run' ((c, x) : acc) cs x Nothing ys
        run' acc (c : cs) x Nothing (y : ys) = run' ((c, x) : acc) cs x (Just y) ys
        run' acc (c : cs) x (Just (AddX a)) ys = run' ((c, x) : acc) cs (x + a) Nothing ys
        run' acc _ _ Nothing [] = reverse acc
        run' _ _ _ _ _ = error "fault"

pixel :: (Cycle, Int) -> Pixel
pixel (c, x)
    | pos `elem` sprite = "#"
    | otherwise = "."
    where
        pos :: Position
        pos = (c - 1) `rem` 40
        sprite :: [Position]
        sprite = [x - 1, x, x + 1]
 
render :: [(Cycle, Int)] -> String
render xs = render' "" $ concatMap pixel xs
    where
        render' :: String -> String -> String
        render' acc "" = acc
        render' acc s = render' (acc ++ "\n" ++ (take 40 s)) (drop 40 s)

day10 :: IO ()
day10 = do
    xs <- parseLines <$> slurpLines "day10.txt"
    let output = run xs
    let interesting = [20, 60, 100, 140, 180, 220]
    let r1 = sum $ map (\ (c, x) -> c * x) $ filter (\ (c, _) -> c `elem` interesting) output
    putStrLn ("Day 10, part 1: " ++ (show r1))
    let r2 = render output
    putStrLn ("Day 10, part 2: " ++ r2)

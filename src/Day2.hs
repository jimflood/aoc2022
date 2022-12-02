module Day2
    ( day2
    ) where

data HandShape = Rock | Paper | Scissors
    deriving (Show)

type Round = (HandShape, HandShape)

data Outcome = Win | Lose | Tie

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> [Round]
parseLines = map parseLines'
    where
        parseLines' :: String -> (HandShape, HandShape)
        parseLines' (a : ' ' : c : _) = (parseLines'' a, parseLines'' c)
        parseLines' _ = error "Nope"
        parseLines'' :: Char -> HandShape
        parseLines'' 'A' = Rock
        parseLines'' 'B' = Paper
        parseLines'' 'C' = Scissors
        parseLines'' 'X' = Rock     -- anyway Lose
        parseLines'' 'Y' = Paper    -- anyway Draw
        parseLines'' 'Z' = Scissors -- anyway Win
        parseLines'' _ = error "Nope"

outcome :: Round -> Outcome
outcome (Rock, Rock) = Tie
outcome (Rock, Paper) = Win
outcome (Rock, Scissors) = Lose
outcome (Paper, Rock) = Lose
outcome (Paper, Paper) = Tie
outcome (Paper, Scissors) = Win
outcome (Scissors, Rock) = Win
outcome (Scissors, Paper) = Lose
outcome (Scissors, Scissors) = Tie

points :: Round -> Integer
points r = (points' (snd r)) + (points'' (outcome r))
    where
        points' :: HandShape -> Integer
        points' Rock = 1
        points' Paper = 2
        points' Scissors = 3
        points'' :: Outcome -> Integer
        points'' Win = 6
        points'' Lose = 0
        points'' Tie = 3

score :: [Round] -> Integer
score rs = sum $ map points rs

anyway :: Round -> Round
anyway (Rock, Rock) = (Rock, Scissors)
anyway (Rock, Paper) = (Rock, Rock)
anyway (Rock, Scissors) = (Rock, Paper)
anyway (Paper, Rock) = (Paper, Rock)
anyway (Paper, Paper) = (Paper, Paper)
anyway (Paper, Scissors) = (Paper, Scissors)
anyway (Scissors, Rock) = (Scissors, Paper)
anyway (Scissors, Paper) = (Scissors, Scissors)
anyway (Scissors, Scissors) = (Scissors, Rock)

day2 :: IO ()
day2 = do
    xs <- fmap parseLines $ slurpLines "day2.txt"
    let r1 = score xs
    putStrLn ("Day 2, part 1: " ++ (show r1))
    let r2 = score (map anyway xs)
    putStrLn ("Day 2, part 2: " ++ (show r2))

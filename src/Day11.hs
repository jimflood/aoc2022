module Day11
    ( day11
    ) where

import Data.List (sort)
import Data.List.Split (splitOneOf)
import qualified Data.Map as Map

type Monkey= Int

type Round = Int

type WorryLevel = Integer

type Item = (Monkey, WorryLevel)

data Behavior = Behavior {
    op :: (WorryLevel -> WorryLevel),
    test :: WorryLevel,
    onTrue :: Monkey,
    onFalse :: Monkey
}

type BehaviorMap = Map.Map Monkey Behavior

type InspectionMap = Map.Map Monkey Int

type CalmingFunction = (WorryLevel -> WorryLevel)

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

ltrim :: String -> String
ltrim = dropWhile (`elem` " \t")

split :: String -> [String]
split = splitOneOf " :,"

tokenize :: String -> [String]
tokenize cs = ((filter (not . null)) . split . ltrim) cs

mapify :: [a] -> Map.Map Int a
mapify xs = Map.fromList $ zip [0..] xs

parseLines :: [String] -> ([Item], BehaviorMap)
parseLines xs = parseLines' [] [] $ filter (not . null) (map tokenize xs)
    where
        parseLines' :: [[Item]] -> [Behavior] -> [[String]] -> ([Item], BehaviorMap)
        parseLines' acci accb [] = (concat (reverse acci), mapify (reverse accb))
        parseLines' acci accb (a : b : c : d : e : f : gs) = parseLines' (items : acci) (behavior : accb) gs
            where
                -- messy but effective
                items :: [Item]
                items = items' (parseA a) (parseB b)
                items' :: Monkey -> [WorryLevel] -> [Item]
                items' m ws = map (\ w -> (m, w)) ws
                parseA :: [String] -> Monkey
                parseA ("Monkey": n : _) = read n :: Monkey
                parseA _ = error "bad input"
                parseB :: [String] -> [WorryLevel]
                parseB (_ : _ : ns) = map (\ n -> read n :: WorryLevel) ns
                parseB _ = error "bad input"
                behavior :: Behavior
                behavior = Behavior (parseC c) (parseD d) (parseE e) (parseF f)
                parseC :: [String] -> (WorryLevel -> WorryLevel)
                parseC ("Operation" : "new" : "=" : "old" : "*" : "old" : _) = (\ x -> x * x)
                parseC ("Operation" : "new" : "=" : "old" : "*" : n : _) = (\ x -> x * (read n :: WorryLevel))
                parseC ("Operation" : "new" : "=" : "old" : "+" : n : _) = (\ x -> x + (read n :: WorryLevel))
                parseC _ = error "bad input"
                parseD :: [String] -> WorryLevel
                parseD ("Test" : "divisible" : "by" : n : _) = read n :: WorryLevel
                parseD _ = error "bad input"
                parseE :: [String] -> Monkey
                parseE ("If" : "true" : "throw" : "to" : "monkey" : n : _) = read n :: Monkey
                parseE _ = error "bad input"
                parseF :: [String] -> Monkey
                parseF ("If" : "false" : "throw" : "to" : "monkey" : n : _) = read n :: Monkey
                parseF _ = error "bad input"
        parseLines' _ _ _ = error "bad input"

turn :: CalmingFunction -> BehaviorMap -> Monkey -> (InspectionMap, [Item]) -> (InspectionMap, [Item])
turn calm bm = turn' [] []
    where
        turn' :: [Item] -> [Item] -> Monkey -> (InspectionMap, [Item]) -> (InspectionMap, [Item])
        turn' throws skips _ (im, []) = (im, (reverse skips) ++ (reverse throws))
        turn' throws skips i (im, (m, w) : xs)
            | m /= i = turn' throws ((m, w) : skips) i (im, xs)
            | otherwise = turn'' (bm Map.! m)
            where
                turn'' :: Behavior -> (InspectionMap, [Item])
                -- part 1: turn'' b = turn''' (((op b) w) `div` 3)
                turn'' b = turn''' (calm ((op b) w))
                    where
                        turn''' :: WorryLevel -> (InspectionMap, [Item])
                        turn''' nw = turn'''' (nw `rem` (test b) == 0)
                            where
                                turn'''' :: Bool -> (InspectionMap, [Item])
                                turn'''' True = go ((onTrue b), nw)
                                turn'''' False = go ((onFalse b), nw)
                                go :: Item -> (InspectionMap, [Item])
                                go a = turn' (a : throws) skips i (Map.insertWith (+) i 1 im, xs)

run :: Round -> CalmingFunction -> BehaviorMap -> [Item] -> InspectionMap
run numRounds calm bm items = run' 0 1 (Map.empty, items)
    where
        run' :: Monkey -> Round -> (InspectionMap, [Item]) -> InspectionMap
        run' i r t
            | i < (Map.size bm) = run' (i + 1) r (turn calm bm i t)
            | r < numRounds = run' 0 (r + 1) t
            | otherwise = fst t

monkeyBusiness :: InspectionMap -> Int
monkeyBusiness im = product $ take 2 $ reverse $ sort $ map snd $ Map.toList im

calm1 :: WorryLevel -> WorryLevel
calm1 w = w `div` 3

calm2 :: BehaviorMap -> WorryLevel -> WorryLevel
-- this was tough to figure out!
calm2 behaviors w = calm2' $ foldl (*) 1 (map (test . snd) (Map.toList behaviors))
    where
        calm2' :: WorryLevel -> WorryLevel
        calm2' f = w `rem` f

day11 :: IO ()
day11 = do
    (items, behaviors) <- parseLines <$> slurpLines "day11.txt"
    let r1 = monkeyBusiness $ run 20 calm1 behaviors items
    putStrLn ("Day 11, part 1: " ++ (show r1))
    let r2 = monkeyBusiness $ run 10000 (calm2 behaviors) behaviors items
    putStrLn ("Day 11, part 2: " ++ (show r2))

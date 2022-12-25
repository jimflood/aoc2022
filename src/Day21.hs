module Day21
    ( day21
    ) where

import Data.List.Split (splitOneOf)
import qualified Data.Map as Map

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

toInt :: String -> Int
toInt a = read a :: Int

type Symbol = String

type Value = Int

type BinaryOp = (Value -> Value -> Value)

data Expr = ValExpr Value | OpExpr Symbol BinaryOp Symbol   

type SymbolMap = Map.Map Symbol Expr

toOp :: String -> BinaryOp
toOp "+" = (+)
toOp "-" = (-)
toOp "*" = (*)
toOp "/" = (div)
toOp _ = error "bad op"

parseLines :: [String] -> SymbolMap
parseLines css = foldl parse Map.empty (map (splitOneOf " :") css)
    where
        parse :: SymbolMap -> [String] -> SymbolMap
        parse m (a : "" : c : d : e : _) = Map.insert a (OpExpr c (toOp d) e) m
        parse m (a : "" : c : _) = Map.insert a (ValExpr (toInt c)) m
        parse _ _ = error "nope"

solve :: SymbolMap -> Symbol -> Value
solve m s = solve' (m Map.! s)
    where
        solve' :: Expr -> Value
        solve' (ValExpr n) = n
        solve' (OpExpr a op b) = op (solve m a) (solve m b)

part1 :: SymbolMap -> Value
part1 m = solve m "root"

part2 :: SymbolMap -> Value
part2 m = part2' [0, 10000000000000]
    where
        part2' :: [Int] -> Value
        part2' [a, b] = part2'' $ map (\ x -> solve (monkeyPatch m x) "root") [a, b]
            where
                part2'' :: [Int] -> Value
                -- simple linear solution
                part2'' [c, d] = least (round ((fromIntegral b) * ((fromIntegral c) / (fromIntegral (c - d)))))
                least :: Int -> Int
                -- multiple solutions -- pick the least
                least n
                    | (solve (monkeyPatch m n) "root" == 0) && (solve (monkeyPatch m (n - 1)) "root" /= 0) = n
                    | otherwise = least (n - 1)

monkeyPatch :: SymbolMap -> Value -> SymbolMap
monkeyPatch m v = monkeyPatch' (m Map.! "root")
    where
        monkeyPatch' :: Expr -> SymbolMap
        monkeyPatch' (OpExpr a _ b) = (Map.insert "root" (OpExpr a (-) b) (Map.insert "humn" (ValExpr v) m))
        monkeyPatch' _ = error "nope"

day21 :: IO ()
day21 = do
    m <- fmap parseLines $ slurpLines "day21.txt"
    let r1 = part1 m
    putStrLn ("Day 21, part 1: " ++ (show r1))
    let r2 = part2 m
    putStrLn ("Day 21, part 2: " ++ (show r2))

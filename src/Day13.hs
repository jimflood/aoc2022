module Day13
    ( day13
    ) where

import Data.List (elemIndex, sortBy)

data PacketData = PacketInt Int | PacketList [PacketData]
    deriving (Show)

instance Eq PacketData where
    a == b = cmpPkts a b == '='

type Packet = PacketData

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> [(Packet, Packet)]
parseLines = (parseLines' []) . (filter (not . null))
    where
        parseLines' :: [(Packet, Packet)] -> [String] -> [(Packet, Packet)]
        parseLines' acc [] = reverse acc
        parseLines' acc (a : b : cs) = parseLines' ((parse a, parse b) : acc) cs
        parseLines' _ _ = error "bad input"

parse :: String -> PacketData
parse = parse' [PacketList []]
    where
        parse' :: [PacketData] -> String -> PacketData
        parse' [a] [] = a
        parse' _ [] = error "bad input"
        parse' acc (x : xs)
            | x == '[' = parse' ((PacketList []) : acc) xs
            | x == ']' = parse' ((into (head acc) (head (tail acc))) : (tail (tail acc))) xs
            | x == ',' = parse' acc xs
            | x >= '0' && x <= '9' = parse'' (read [x] :: Int) xs
                where
                    into :: PacketData -> PacketData -> PacketData
                    into a (PacketList bs) = PacketList (bs ++ [a])
                    into _ _ = error "bad input"
                    parse'' :: Int -> String -> PacketData
                    parse'' n (y : ys)
                        | y >= '0' && y <= '9' = parse'' ((n * 10) + (read [y] :: Int)) ys
                        | otherwise = parse' (((into (PacketInt n) (head acc))) : (tail acc)) (y : ys)
                    parse'' _ _ = error "bad input"    
        parse' _ _ = error "bad input"

cmpPkts :: PacketData -> PacketData -> Char
cmpPkts (PacketInt a) (PacketInt b)
    | a < b = '<'
    | a > b = '>'
    | otherwise = '='
cmpPkts (PacketInt a) (PacketList bs) = cmpPkts (PacketList [(PacketInt a)]) (PacketList bs)
cmpPkts (PacketList as) (PacketInt b) = cmpPkts (PacketList as) (PacketList [(PacketInt b)])
cmpPkts (PacketList []) (PacketList (_ : _)) = '<'    
cmpPkts (PacketList (_ : _)) (PacketList []) = '>'    
cmpPkts (PacketList []) (PacketList []) = '='    
cmpPkts (PacketList (a : as)) (PacketList (b : bs)) = cmpPkts'' (cmpPkts a b)
    where
        cmpPkts'' :: Char -> Char
        cmpPkts'' '=' = cmpPkts (PacketList as) (PacketList bs)
        cmpPkts'' x = x

orderPkts :: PacketData -> PacketData -> Ordering
orderPkts a b
    | cmpPkts a b == '<' = LT
    | otherwise = GT

day13 :: IO ()
day13 = do
    xs <- parseLines <$> slurpLines "day13.txt"
    let cmp1 = map (\ (a, b) -> cmpPkts a b) xs
    let idx1 = map fst $ filter (\ (_, b) -> b == '<') $ zip [1..] cmp1
    let r1 = sum idx1
    putStrLn ("Day 13, part 1: " ++ (show r1))
    let pkts = concatMap (\ (a, b) -> [a, b]) xs
    let d1 = parse "[[2]]"
    let d2 = parse "[[6]]"
    let sorted = sortBy orderPkts (d1 : d2 : pkts)
    let (Just i1) = elemIndex d1 sorted
    let (Just i2) = elemIndex d2 sorted
    let r2 = (i1 + 1) * (i2 + 1)
    putStrLn ("Day 13, part 2: " ++ (show r2))

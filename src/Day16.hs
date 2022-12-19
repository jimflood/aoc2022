module Day16
    ( day16
    ) where

import Data.List
import Data.List.Split
import qualified Data.Map as Map

type Valve = String

type Pressure = Int

type Distance = Int

data Room = Room {
    valve :: Valve,
    rate :: Pressure,
    tunnels :: [Valve]
} deriving (Show)

type RateMap = Map.Map Valve Pressure

type PathMap = Map.Map Valve [Valve]

type DistanceMap = Map.Map (Valve, Valve) Distance

type Minute = Int

data Entity = Entity {
    walking :: Maybe Distance,
    opening :: Maybe Pressure,
    heading :: Valve
} deriving (Eq, Show)

data Scenario = Scenario {
    entities :: [Entity],
    minutes :: Minute,
    pressure :: Pressure,
    totalRate :: Pressure,
    chosen :: [Valve]
} deriving (Show)

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> [Room]
parseLines css = map parse css
    where
        parse :: String -> Room
        parse = (parse' . (splitOneOf " =;,"))
        parse' :: [String] -> Room
        -- ["Valve","HH","has","flow","rate","22","","tunnel","leads","to","valve","GG"]
        parse' ("Valve" : a : _ : _ : "rate" : b : _ : _ : _ : "to" : _ : xs) = parse'' a b xs
        parse' xx = error (show xx)
        parse'' :: String -> String -> [String] -> Room
        parse'' v r ts = Room v (read r :: Int) (filter (not . null) ts)

ratesOf :: [Room] -> RateMap
ratesOf rooms = Map.fromList $ map (\ x -> (valve x, rate x)) rooms

pathsOf :: [Room] -> PathMap
pathsOf rooms = Map.fromList $ map (\x -> (valve x, tunnels x)) rooms

distancesOf :: RateMap -> PathMap -> DistanceMap
distancesOf rm pm = Map.fromList $ concatMap measure $ parings (filter interesting $ Map.keys pm)
    where
        interesting :: Valve -> Bool
        interesting x = x == "AA" || rm Map.! x > 0
        parings :: [Valve] -> [(Valve, Valve)]
        parings ks = [(x, y) | x <- ks, y <- ks , x < y]
        measure :: (Valve, Valve) -> [((Valve, Valve), Distance)]
        measure (a, b) = measure' 1 [] (pm Map.! a)
            where
                measure' :: Distance -> [Valve] -> [Valve] -> [((Valve, Valve), Distance)]
                measure' n vs xs
                    | b `notElem` xs = measure' (n + 1) (vs ++ xs) (explode xs \\ (vs ++ xs))
                    | a == "AA" = [((a, b), n)] -- one way
                    | otherwise = [((a, b), n), ((b, a), n)]
                explode :: [Valve] -> [Valve]
                explode xs = concatMap (\ x -> pm Map.! x) xs

flowable :: RateMap -> [Valve]
flowable rm = map fst $ filter (\ (_, r) -> r > 0) $ Map.toList rm

run :: [Valve] -> RateMap -> DistanceMap -> Scenario -> [Scenario]
run fs rm dm s = run' [] [s]
    where
        run' :: [Scenario] -> [Scenario] -> [Scenario]
        run' acc [] = acc
        run' acc xs = run'' (prune (map update (map advance (concatMap branch xs))))
            where
                run'' :: [Scenario] -> [Scenario]
                run'' ys = run' (acc ++ (filter wheat ys)) (filter (not . wheat) ys)
                wheat :: Scenario -> Bool
                wheat x = (minutes x == 0)
        prune :: [Scenario] -> [Scenario]
        -- prune amount by trial and error
        prune xs = take 1000 $ reverse $ sortOn (\ x -> (pressure x) + ((totalRate x) * (minutes x))) $ filter (\x -> minutes x >= 0) $ xs
        advance :: Scenario -> Scenario
        advance (Scenario es m p tr cs) = (Scenario es (m - 1) (p + tr) tr cs)
        update :: Scenario -> Scenario
        update = update' []
            where
                update' :: [Entity] -> Scenario -> Scenario
                update' acc (Scenario [] m p tr cs) = (Scenario acc m p tr cs)
                update' acc (Scenario ((Entity (Just a) b c) : es) m p tr cs)
                    | a == 1 = update' ((Entity Nothing b c) : acc) (Scenario es m p tr cs)
                    | otherwise = update' ((Entity (Just (a - 1)) b c) : acc) (Scenario es m p tr cs)
                update' acc (Scenario ((Entity Nothing (Just _) c) : es) m p tr cs)
                    | True = update' ((Entity Nothing Nothing c) : acc) (Scenario es m p (tr + (rm Map.! c)) cs)
                update' acc (Scenario (e : es) m p tr cs) = update' (e : acc) (Scenario es m p tr cs)

        branch :: Scenario -> [Scenario]
        branch (Scenario es m p tr cs) = branch' (find chosable es)
            where
                noop = [(Scenario es m p tr cs)]
                chosable :: Entity -> Bool
                chosable (Entity Nothing Nothing _) = True
                chosable _ = False
                branch' :: Maybe Entity -> [Scenario]
                branch' Nothing = noop
                branch' (Just e)
                    | null (fs \\ cs) = noop
                    | otherwise = concatMap branch $ branch'' (fs \\ cs)
                    where
                        branch'' :: [Valve] -> [Scenario]
                        branch'' [] = error "cannot occur"
                        branch'' vs = map branch''' vs
                            where
                                branch''' :: Valve -> Scenario
                                branch''' v = Scenario ((go v) : (es \\ [e])) m p tr (v : cs)
                                go :: Valve -> Entity
                                go v = Entity (Just (dm Map.! (heading e, v))) (Just (rm Map.! v)) v

day16 :: IO ()
day16 = do
    rooms <- fmap parseLines $ slurpLines "day16.txt" -- _sample.txt"
    let rm = ratesOf rooms
    let pm = pathsOf rooms
    let dm = distancesOf rm pm
    let start = Scenario [(Entity Nothing Nothing "AA")] 30 0 0 []
    let fs = flowable rm
    let x1 = run fs rm dm start
    let yy = map (\ x -> (pressure x) + ((totalRate x) * (minutes x))) x1
    let r1 = maximum yy
    putStrLn ("Day 16, part 1: " ++ (show r1))
    let start2 = Scenario [(Entity Nothing Nothing "AA"), (Entity Nothing Nothing "AA")] 26 0 0 []
    let x2 = run fs rm dm start2
    let yy2 = map (\ x -> (pressure x) + ((totalRate x) * (minutes x))) x2
    let r2 = maximum yy2
    putStrLn ("Day 16, part 2: " ++ (show r2))

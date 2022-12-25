module Day22
    ( day22
    ) where

import Data.Tuple (swap)
import Data.List.Split (oneOf, split)
import qualified Data.Map as Map

type Coordinate = (Int, Int)

data Tile = OpenTile | SolidTile
    deriving (Eq, Show)

type BoardMap = Map.Map Coordinate Tile

data PathSegment = MoveForward Int | TurnLeft | TurnRight
    deriving (Show)

type Path = [PathSegment]

data Facing = FacingUp | FacingDown | FacingLeft | FacingRight
    deriving (Show)

data Face = FrontFace | BackFace | TopFace | BottomFace | LeftFace | RightFace
    deriving (Show)

type Entity = (Coordinate, Facing)

slurpLines :: String -> IO [String]
slurpLines filename = lines <$> readFile filename

parseLines :: [String] -> (BoardMap, Path)
parseLines = parse 1 []
    where
        parse :: Int -> [(Coordinate, Tile)] -> [String] -> (BoardMap, Path)
        parse _ acc ("" : p : _) = parsePath (Map.fromList acc) p
        parse y acc (cs : css) = parse (y + 1) (foldl parseTile acc (zip [1..] cs)) css
            where
                parseTile :: [(Coordinate, Tile)] -> (Int, Char) -> [(Coordinate, Tile)]
                parseTile ts (x, '.') = ((x, y), OpenTile) : ts
                parseTile ts (x, '#') = ((x, y), SolidTile) : ts
                parseTile ts (_, ' ') = ts
                parseTile _ _ = error "bad input"
        parse _ _ _ = error "bad input"
        parsePath :: BoardMap -> String -> (BoardMap, Path)
        parsePath bm p = parsePath' [] (split (oneOf " RL") p)
            where
                parsePath' :: [PathSegment] -> [String] -> (BoardMap, Path)
                parsePath' acc [] = (bm, reverse acc)
                parsePath' acc ("R" : xs) = parsePath' (TurnRight : acc) xs
                parsePath' acc ("L" : xs) = parsePath' (TurnLeft : acc) xs
                parsePath' acc (a : xs) = parsePath' ((MoveForward (read a :: Int)) : acc) xs

{-
display :: BoardMap -> IO ()
display bm = display' $ Map.keys bm
    where
        display' :: [Coordinate] -> IO ()
        display' xys = display'' (maximum (map fst xys)) (maximum (map snd xys))
        display'' :: Int -> Int -> IO ()
        display'' maxX maxY = displayLine 1
            where
                displayLine :: Int -> IO ()
                displayLine y
                    | y > maxY = return ()
                    | otherwise = displayTile 1
                    where
                        displayTile :: Int -> IO ()
                        displayTile x
                            | x > maxX = do
                                putStrLn ""
                                displayLine (y + 1)
                            | Map.notMember (x, y) bm = do
                                putStr " "
                                displayTile (x + 1)
                            | (bm Map.! (x, y)) == OpenTile = do
                                putStr "."
                                displayTile (x + 1)
                            | otherwise = do
                                putStr "#"
                                displayTile (x + 1)
-}

wrapOf :: BoardMap -> Entity -> Entity
wrapOf bm ((x, y), f) = (wrapOf' f, f)
    where
        wrapOf' :: Facing -> Coordinate
        wrapOf' FacingUp = wrapOf'' maximum snd fst x swap
        wrapOf' FacingLeft = wrapOf'' maximum fst snd y id
        wrapOf' FacingDown = wrapOf'' minimum snd fst x swap
        wrapOf' FacingRight = wrapOf'' minimum fst snd y id
        wrapOf'' f1 f2 f3 n f4 = f4 (f1 (map f2 $ filter (\ t -> (f3 t) == n) (Map.keys bm)), n)

cubeWrapOf :: BoardMap -> Entity -> Entity
cubeWrapOf _ ((x, y), f)
    | (x > 50) && (x <= 100) && (y <= 50) = cubeWrapFr f
    | (x > 100) && (y <= 50) = cubeWrapRi f
    | (x > 50) && (y <= 100) = cubeWrapBo f
    | (x > 50) = cubeWrapBa f
    | (y <= 150) = cubeWrapLe f
    | otherwise = cubeWrapTo f
    where
        cubeWrapFr FacingUp = ((1, x + 100), FacingRight)
        cubeWrapFr FacingDown = ((x, y + 1), FacingDown)
        cubeWrapFr FacingRight = ((x + 1, y), FacingRight)
        cubeWrapFr FacingLeft = ((1, 151 - y), FacingRight)

        cubeWrapRi FacingUp = ((x - 100, 200), FacingUp)
        cubeWrapRi FacingDown = ((100, x - 50), FacingLeft)
        cubeWrapRi FacingRight = ((100, 151 - y), FacingLeft)
        cubeWrapRi FacingLeft = ((x - 1, y), FacingLeft)

        cubeWrapTo FacingUp = ((x, y - 1), FacingUp)
        cubeWrapTo FacingDown = ((x + 100, 1), FacingDown)
        cubeWrapTo FacingRight =((y - 100, 150), FacingUp)
        cubeWrapTo FacingLeft = ((y - 100, 1), FacingDown)

        cubeWrapLe FacingUp = ((51, x + 50), FacingRight)
        cubeWrapLe FacingDown = ((x, y + 1), FacingDown)
        cubeWrapLe FacingRight = ((x + 1, y), FacingRight)
        cubeWrapLe FacingLeft = ((51, 151 - y), FacingRight)

        cubeWrapBo FacingUp = ((x, y - 1), FacingUp)
        cubeWrapBo FacingDown = ((x, y + 1), FacingDown)
        cubeWrapBo FacingRight = ((y + 50, 50), FacingUp)
        cubeWrapBo FacingLeft = ((y - 50, 101), FacingDown)

        cubeWrapBa FacingUp = ((x, y - 1), FacingUp)
        cubeWrapBa FacingDown = ((50, x + 100), FacingLeft)
        cubeWrapBa FacingRight = ((150, 151 - y), FacingLeft)
        cubeWrapBa FacingLeft = ((x - 1, y), FacingLeft)

topLeftmost :: BoardMap -> Entity
topLeftmost bm = wrapOf bm ((1, 1), FacingRight)

solve :: (BoardMap -> Entity -> Entity) -> BoardMap -> Entity -> Path -> Entity
solve wrapf bm e0 p= foldl solve' e0 p
    where
        solve' :: Entity -> PathSegment -> Entity
        solve' e TurnLeft = (fst e, turnLeft (snd e))
        solve' e TurnRight = (fst e, turnRight (snd e))
        solve' e (MoveForward a) = moveForward a e
        turnLeft :: Facing -> Facing
        turnLeft FacingUp = FacingLeft
        turnLeft FacingLeft = FacingDown
        turnLeft FacingDown = FacingRight
        turnLeft FacingRight = FacingUp
        turnRight :: Facing -> Facing
        turnRight FacingUp = FacingRight
        turnRight FacingRight = FacingDown
        turnRight FacingDown = FacingLeft
        turnRight FacingLeft = FacingUp
        moveForward :: Int -> Entity -> Entity
        moveForward 0 e = e
        moveForward n ((x, y), f) = moveForward' ((step f), f)
            where
                moveForward' :: Entity -> Entity
                moveForward' ((a, b), ff)
                    | Map.member (a, b) bm = moveForward'' ((a, b), ff)
                    | otherwise = moveForward'' (wrapf bm ((x, y), f))
                moveForward'' :: Entity -> Entity
                moveForward'' ((nx, ny), ff)
                    | Map.notMember (nx, ny) bm = error "bug" 
                    | bm Map.! (nx, ny) == OpenTile = moveForward (n - 1) ((nx, ny), ff)
                    | otherwise = ((x, y), f) -- blocked
                step :: Facing -> Coordinate
                step FacingUp = (x, y - 1)
                step FacingRight = (x + 1, y)
                step FacingDown = (x, y + 1)
                step FacingLeft = (x - 1, y)

fValue :: Facing -> Int
fValue FacingRight = 0
fValue FacingDown = 1
fValue FacingLeft = 2
fValue FacingUp = 3

day22 :: IO ()
day22 = do
    (bm, p) <- fmap parseLines $ slurpLines "day22.txt"
    let e = topLeftmost bm
    let ((ex, ey), ef) = solve wrapOf bm e p
    let r1 = (ey * 1000) + (ex * 4) + (fValue ef)
    putStrLn ("Day 22, part 1: " ++ (show r1))
    let ((ex2, ey2), ef2) = solve cubeWrapOf bm e p
    let r2 = (ey2 * 1000) + (ex2 * 4) + (fValue ef2)
    putStrLn ("Day 22, part 2: " ++ (show r2))

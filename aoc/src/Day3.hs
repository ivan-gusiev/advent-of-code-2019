{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day3 (
    manhattanDistance, manhattanDistance'
  , mkSegment, parsePath
  , mkWire
  , examine, intersections, closestIntersection
  , annotate, enumerate, closestIntersectionBySteps
  , drawAnnotation
  ) where

    import Data.List.Split(splitOn)
    import Data.Maybe(catMaybes, isJust)
    import qualified Data.Map as DM
    import Data.Ord(comparing)
    import Data.Sort(sortOn, sort)
    import Safe(headMay)

    type Direction = Char
    type PSegment = (Direction, Int)
    type Path = [PSegment]

    data WSegment = WSegment { x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int }
        deriving (Eq, Show)
    type Wire = [WSegment]
    type Point = (Int, Int)
    type Annotation = DM.Map Point Int

    manhattanDistance :: Int -> Int -> Int -> Int -> Int
    manhattanDistance x1 y1 x2 y2 = abs (x2 - x1) + abs (y2 - y1)

    manhattanDistance' :: Point -> Point -> Int
    manhattanDistance' (x1, y1) (x2, y2) = manhattanDistance x1 y1 x2 y2

    mkSegment :: Int -> Int -> Int -> Int -> WSegment
    mkSegment = WSegment

    parsePath :: String -> Path
    parsePath = map parseSegment . splitOn [',']
        where parseSegment :: String -> PSegment
              parseSegment (d : len) = (d, read len)

    mkWire :: Path -> Wire
    mkWire = foldl nextSegment []
        where 
            putWire x y (dir, len) = 
                let mk = WSegment x y in case dir of
                'U' -> mk x (y + len)
                'D' -> mk x (y - len)
                'L' -> mk (x - len) y
                'R' -> mk (x + len) y
            nextSegment [] pseg = [putWire 0 0 pseg]
            nextSegment (h : t) pseg = 
                let (x, y) = (x2 h, y2 h) in
                putWire x y pseg : h : t
    
    examine :: WSegment -> WSegment -> Maybe Point
    examine lseg rseg =
        case (vert lseg, vert rseg) of
            (True, True) -> Nothing
            (False, False) -> Nothing
            (True, False) -> 
                let x = x1 lseg in
                let y = y1 rseg in
                if inside x (x1 rseg) (x2 rseg) && inside y (y1 lseg) (y2 lseg) then
                    Just (x, y)
                else
                    Nothing
            (False, True) ->
                let x = x1 rseg in
                let y = y1 lseg in
                if inside x (x1 lseg) (x2 lseg) && inside y (y1 rseg) (y2 rseg) then
                    Just (x, y)
                else
                    Nothing
        where
            vert seg = x1 seg == x2 seg
            inside a x y = a > min x y && a < max x y

    intersections :: Wire -> Wire -> [Point]
    intersections lwire rwire =
        catMaybes [ examine l r | l <- lwire, r <- rwire ]
    
    closestIntersection :: Wire -> Wire -> Maybe Point
    closestIntersection l r =
        headMay $ sortOn (manhattanDistance' (0, 0)) (intersections l r)
    
    annotate :: Wire -> Annotation
    annotate input =
        let wire = reverse input in
        let (_, _, result) = foldl nextPart ((0, 0), 0, DM.empty) wire in
        result
        where
            nextPart :: (Point, Int, Annotation) -> WSegment -> (Point, Int, Annotation)
            nextPart (pt, i, map) seg =
                let indexedSteps :: [(Point, Int)] = zip (enumerate pt seg) [i..] in
                --let isteps = trace (show (head indexedSteps, last indexedSteps)) indexedSteps in
                foldl updateMap (pt, i, map) indexedSteps
            updateMap :: (Point, Int, Annotation) -> (Point, Int) -> (Point, Int, Annotation)
            updateMap (cur, _, map) (pt, i) = 
                (pt, i, DM.alter setMin pt map)
                where
                    setMin Nothing = Just i
                    setMin (Just x) = Just (min i x)

    enumerate :: Point -> WSegment -> [Point]
    enumerate (curx, cury) seg
        | x1 seg == curx && y1 seg == cury && y2 seg == cury = 
            map (, cury) (range (x1 seg) (x2 seg))
        | x1 seg == curx && y1 seg == cury = 
                map (curx, ) (range (y1 seg) (y2 seg))
        | x2 seg == curx && y2 seg == cury && y1 seg == cury = 
            map (, cury) (range (x2 seg) (x1 seg))
        | x2 seg == curx && y2 seg == cury = 
            map (curx, ) (range (y2 seg) (y1 seg))
        | otherwise = error $ show (x1 seg, y1 seg, x2 seg, y2 seg, curx, cury)
            where 
                range :: Int -> Int -> [Int]
                range a b  = if a < b then [a..b] else reverse [b..a]

    closestIntersectionBySteps :: Wire -> Wire -> Maybe Int
    closestIntersectionBySteps l r =
        let lann = annotate l in
        let rann = annotate r in
        let is = intersections l r in
        let steps = map (\pt -> 
                            let lsteps = DM.findWithDefault 10000 pt lann in
                            let rsteps = DM.findWithDefault 10000 pt rann in
                                lsteps + rsteps
                            ) is in
        headMay $ sort steps
    
    drawAnnotation :: Annotation -> IO ()
    drawAnnotation ann =
        let xMax = maximum (map (\((x, y), _) -> x) (DM.toList ann)) in
        let yMax = maximum (map (\((x, y), _) -> y) (DM.toList ann)) in
        let printRow row = map (\x -> do
                                        let value = DM.lookup (x, row) ann
                                        if isJust value then last (show value) else '.') [0..xMax + 1] in
        let rows = map printRow (reverse [0..yMax + 1]) in
        mapM_ putStrLn rows
import Data.Array
import Data.List(nub, unfoldr)


getAllLine :: IO [String]
getAllLine = do
    line <- getLine
    if null line then return []
                 else fmap (line : ) getAllLine

type Coord = (Int, Int)
processBoard :: [[Char]] -> Array Coord Char
processBoard l = listArray ((1, 1), (nRow, nCol)) (concat l)
    where nRow = length l
          nElemFirstRow = length (head l)
          nCol
            | null l = 0
            | all ((==nElemFirstRow) . length) l = nElemFirstRow
            | otherwise = error "board have different count across rows"

getAllAntennaPairs :: Array Coord Char -> [(Coord, Coord)]
getAllAntennaPairs board = let antennas = filter (\(_, v) -> v /= '.') $ assocs board
                            in [ (x, y)
                               | (x, vx) <- antennas, (y, vy) <- antennas
                               , vx == vy , x /= y
                               ]


task1 :: Array Coord Char -> Int
task1 board = let pairs = map (uncurry getAntinode) $ getAllAntennaPairs board
               in length $ filter (inRange (bounds board)) (nub pairs)

getAntinode :: Coord -> Coord -> Coord
getAntinode (x1, y1) (x2, y2) = (2 * x2 - x1, 2 * y2 - y1)

getHarmonic :: Coord -> Coord -> [Coord]
getHarmonic c1 c2 = unfoldr genNext (c1, c2)
    where genNext (c1, c2) = let nx = getAntinode c1 c2
                              in Just (c2, (c2, nx))

task2 :: Array Coord Char -> Int
task2 board = let pairs = map (uncurry getHarmonic) $ getAllAntennaPairs board
               in length . nub . concatMap (takeWhile (inRange (bounds board))) $ pairs

main = do
    board <- processBoard <$> getAllLine
    print $ task1 board
    print $ task2 board



    


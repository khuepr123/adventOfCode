import Data.Foldable
import Data.Array
import Data.List
import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as Mp
import Data.Maybe(fromMaybe)


getAllLine :: IO [String]
getAllLine = do
    line <- getLine
    if null line then return []
                 else fmap (line : ) getAllLine

readRules :: String -> (Int, Int)
readRules s = let [x, y] = readIntsWith "|" s
               in (x, y)

readIntsWith :: String -> String -> [Int]
readIntsWith ptr s = map read $ splitOn ptr s 


newtype Direction = Dir Char

makeDirection :: Char -> Direction
makeDirection c
  | c `elem` ['U', 'R', 'D', 'L'] = Dir c
  | otherwise  = error "bad direction"

rotate90 :: Direction -> Direction
rotate90 (Dir x) = Dir (convert90 x)
      where convert90 'U' = 'R'
            convert90 'R' = 'D'
            convert90 'D' = 'L'
            convert90 'L' = 'U'

move :: Direction -> (Int, Int) -> (Int, Int)
move (Dir 'U') (x, y) = (x-1, y)
move (Dir 'R') (x, y) = (x, y+1)
move (Dir 'D') (x, y) = (x+1, y)
move (Dir 'L') (x, y) = (x, y-1)

data Cell = Obstacle | Clear
type Coord = (Int, Int)
type Board = Array Coord Cell
type Human = (Coord, Direction)

parseBoard :: Char -> Char -> Char -> [[Char]] -> (Board, Human)
parseBoard human obstacle clear l =
    case playerCoords of
        [x] -> (parsedBoard, (x, makeDirection 'U'))
        _   -> error "more than one player present"

    where charBoard = genboard l
          playerCoords = [i | (i, c) <- assocs charBoard, c == human]
          parsedBoard = fmap toCell charBoard
          toCell c
            | c == human    = Clear
            | c == obstacle = Obstacle
            | c == clear    = Clear
            | otherwise     = error "undefined cell"


genboard :: [[a]] -> Array Coord a
genboard l = listArray ((1, 1), (numRows, numCols)) (concat l)
    where (hl, tl)  = fromMaybe (error "empty list in board parsing") (uncons l)
          numRows  = length l
          numCols  = if all ((ncol==) . length) tl 
                        then ncol
                        else error "non-rectangular board passed"
          ncol = length hl
          
nubOrd :: (Foldable t, Ord a) => t a -> Int
nubOrd = S.size . S.fromList . toList

withinBoard :: Human -> Board -> Maybe Cell
withinBoard (loc, _) board 
  | inRange (bounds board) loc = Just (board ! loc)
  | otherwise = Nothing

tryMove :: Board -> Human -> Maybe Human
tryMove board (loc, dir) = case withinBoard nxCell board of
                             Just Obstacle -> Just (loc, rotate90 dir)
                             Just Clear    -> Just nxCell
                             Nothing       -> Nothing
    where nxCell = (move dir loc, dir)

generate :: (a -> Maybe a) -> a -> [a]
generate f = unfoldr (fmap (\x -> (x, x)) . f)

task1 :: [[Char]] -> Int
task1 l =
    let (board, human) = parseBoard '^' '#' '.' l
        trail = human : generate (tryMove board) human
     in nubOrd (map fst trail)

isLoop :: Board -> Human -> Coord -> Bool
isLoop board human blockade
  | blockade == fst human = False
  | otherwise = possibleHumanCount < (length . take (possibleHumanCount + 1) $ generate (tryMove newBoard) human)
      where possibleHumanCount = rangeSize (bounds board) * 4
            newBoard = board // [(blockade, Obstacle)]

task2 l =
    let (board, human) = parseBoard '^' '#' '.' l
        blockable = map fst $ generate (tryMove board) human
     in length $ filter (isLoop board human) (nub blockable)


main = do
    l <- getAllLine
    print $ task1 l
    print $ task2 l


    


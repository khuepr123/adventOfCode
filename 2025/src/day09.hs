import Data.List
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import Control.Exception(assert)
import Data.Array
import Data.List.Split
import Data.Maybe(fromMaybe)
import Debug.Trace

-- get coords
-- get horizontal lines

getAllLine :: IO [String]
getAllLine = do
    line <- getLine
    if null line then return []
                 else fmap (line : ) getAllLine

processLine :: String -> Coordinate
processLine s = let [l, r] = map read . splitOn "," $ s
                 in Coordinate l r

data Coordinate = Coordinate Int Int deriving Show

getSegments l = zip l (last l : init l)


getHorizontalSegment :: [Coordinate] -> [(Coordinate, Coordinate)]
getHorizontalSegment = filter isHorizontal . getSegments

isHorizontal (Coordinate x1 _, Coordinate x2 _) = x1 == x2

minMax :: Ord a => a -> a -> (a, a)
minMax x y = if x < y then (x, y) else (y, x)

getArea :: Coordinate -> Coordinate -> Int
getArea (Coordinate x1 y1) (Coordinate x2 y2) = 
    let (upx, dnx) = minMax x1 x2
        (upy, dny) = minMax y1 y2
     in (dnx - upx + 1) * (dny - upy + 1)

getCoordInRec :: Coordinate -> Coordinate -> [Coordinate]
getCoordInRec (Coordinate x1 y1) (Coordinate x2 y2) = 
    let (upx, dnx) = minMax x1 x2
        (upy, dny) = minMax y1 y2
     in [Coordinate x y | x <- [upx..dnx], y <- [upy..dny]]
-- m a -> (a -> m b) -> m b

getXs :: [Coordinate] -> [Int]
getXs = map (\(Coordinate x _) -> x)
getYs :: [Coordinate] -> [Int]
getYs = map (\(Coordinate _ y) -> y)



getOccupied :: [Coordinate] -> Array (Int, Int) Bool
getOccupied coords = fmap (>0) $ runSTArray $ do

    let segments = getSegments coords
        horizontal = getHorizontalSegment coords
        boundPoints = concatMap (tail . uncurry getCoordInRec) horizontal

    let minX = minimum $ getXs coords
        minY = minimum $ getYs coords
        maxX = maximum $ getXs coords
        maxY = maximum $ getYs coords
        boardIndexs = [Coordinate x y | x <- [minX..maxX] , y <- [minY..maxY]]

    board <- newArray ((minX, minY), (maxX, maxY)) 0

    applyToAllCoord board (+1) boundPoints
    forM_ boardIndexs $ \(Coordinate x y) -> when (x > minX) $ do
        val <- readArray board (x - 1, y)
        target <- readArray board (x, y)
        writeArray board (x, y) (val + target)
    applyToAllCoord board (`mod` 2) boardIndexs
    applyToAllCoord board (const 1) (concatMap (uncurry getCoordInRec) segments)
    return board

        where applyToAllCoord :: STArray s (Int, Int) Int -> (Int -> Int) -> [Coordinate] -> ST s ()
              applyToAllCoord board func coords = 
                  forM_ coords $ \(Coordinate x y) -> do
                      val <- readArray board (x, y)
                      writeArray board (x, y) (func val)
              
board2DtoString :: Array (Int, Int) String -> String
board2DtoString board = 
    let ((minX, minY), (maxX, maxY)) = bounds board
        boardList = [[board ! (x, y) | y <- [minY..maxY]] | x <- [minX..maxX]]
     in unlines . map concat $ boardList

(!?) :: Ix i => Array i e -> i -> Maybe e
arr !? index = if inRange (bounds arr) index
                  then Just (arr ! index)
                  else Nothing

fullRedCheck :: Array (Int, Int) Bool -> Coordinate -> Coordinate -> Bool
fullRedCheck board =
    let ((minX, minY), (maxX, maxY)) = bounds board
        newBoard = listArray ((minX - 1, minY - 1), (maxX, maxY)) [calc x y | x <- [minX-1..maxX], y <- [minY-1..maxY]]
        calc x y = (if fromMaybe False $ board !? (x, y) then 0 else 1) + voidAccess (x - 1) y + voidAccess x (y - 1) - voidAccess (x - 1) (y - 1)
        voidAccess x y = fromMaybe 0 $ newBoard !? (x, y)
        checkRec (Coordinate x1 y1) (Coordinate x2 y2) =
            let (upX, dnX) = minMax x1 x2
                (upY, dnY) = minMax y1 y2
                pestCount 
                  = (newBoard ! (dnX, dnY))
                  + (newBoard ! (upX - 1, upY - 1))
                  - (newBoard ! (upX - 1, dnY))
                  - (newBoard ! (dnX, upY - 1))
             in pestCount == 0
     in checkRec

getBestRectangle :: [Coordinate] -> [(Int, Coordinate, Coordinate)] -> Int
getBestRectangle coords areaAndCoords = 
    let checkfunc = fullRedCheck . getOccupied $ coords
        purified = filter (\(_, p1, p2) -> checkfunc p1 p2) areaAndCoords
     in maximum . map fst3 $ purified
    where fst3 (x, _, _) = x

compressing :: [Coordinate] -> ([Coordinate], Array Int Int, Array Int Int)
compressing coords =
    let cXs = getXs coords
        cYs = getYs coords
        (searchX, arrX) = getSearch cXs
        (searchY, arrY) = getSearch cYs
        newCoords = map (\(Coordinate x y) -> Coordinate (searchX x) (searchY y)) coords
     in (newCoords, arrX, arrY)
    where getSearch :: [Int] -> (Int -> Int, Array Int Int)
          getSearch givenList = let ls = sort $ nub givenList
                                    arr = listArray (1, length ls) ls
                                    search l r val
                                       | l == r    = assert ((arr ! l) == val) l
                                       | otherwise = let mid = (l + r) `div` 2
                                           in if arr ! mid >= val then search l mid val
                                                                  else search (mid + 1) r val
                                 in (search 1 (length ls), arr)

newCoordsAndAreas :: [Coordinate] -> ([Coordinate], [(Int, Coordinate, Coordinate)])
newCoordsAndAreas coords = 
    let (fakeCoords, arrX, arrY) = compressing coords
        realCoord (Coordinate x y) = Coordinate (arrX ! x) (arrY ! y)
        realArea p1 p2 = getArea (realCoord p1) (realCoord p2)
     in (fakeCoords, [(realArea p1 p2, p1, p2) | p1 <- fakeCoords, p2 <- fakeCoords])

main = do
    coords <- map processLine <$> getAllLine
    let (fakeCoords, areas) = newCoordsAndAreas coords
    print $ length fakeCoords
    print fakeCoords
    print $ getBestRectangle fakeCoords areas
    

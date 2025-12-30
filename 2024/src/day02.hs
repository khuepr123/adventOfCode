import Data.List
import qualified Data.HashMap.Strict as Hm
import Data.Maybe(fromMaybe)

getAllLine :: IO [String]
getAllLine = do
    line <- getLine
    if null line then return []
                 else fmap (line : ) getAllLine

processLine :: String -> [Int]
processLine = map read . words

incCheck x y = df > 0 && df <= 3
    where df = y - x

checkCount l = length $ filter (not . uncurry incCheck) pairs
    where pairs = zip l (drop 1 l)

checkFull l = checkCount l == 0 || checkCount (reverse l) == 0

secCalc l = secCheck l || secCheck (reverse l)
    where secCheck l
            | checkFull (tail l) = True
            | checkFull (init l) = True
            | otherwise = let triplets = zip3 l (drop 1 l) (drop 2 l)
                              bestTriplet = minimum $ map winCount triplets
                           in checkCount l + bestTriplet <= 0


winCount (a, b, c) = checkCount [a, c] - checkCount [a, b] - checkCount [b, c]

main = do
    l <- map processLine <$> getAllLine
    let day1 = map (fromEnum . checkFull) l
    print day1
    print $ sum day1
    let day2 = map (fromEnum . secCalc) l
    print day2
    print $ sum day2
 
                



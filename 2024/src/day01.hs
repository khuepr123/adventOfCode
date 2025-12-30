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

zipAll ::[[Int]] -> [[Int]]
zipAll = foldr (zipWith (:)) (repeat [])

calc :: [[Int]] -> Int
calc l =
    let [l1, l2] = zipAll l
     in sum $ zipWith dist (sort l1) (sort l2)
    where dist x y = abs (x - y)

secCalc l =
    let [l1, l2] = zipAll l
        table = Hm.fromListWith (+) (map (,1) l2)
     in sum $ map (simScore table) l1
    where simScore table num = num * (fromMaybe 0 $ Hm.lookup num table)


main = do
    l <- map processLine <$> getAllLine
    print $ calc l
    print $ secCalc l
 
                



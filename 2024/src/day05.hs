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

pairs :: [a] -> [(a, a)]
pairs = concat . unfoldr genPairs
    where genPairs []  = Nothing
          genPairs (h:t) = Just (map (h,) t, t)

findWith0 = Mp.findWithDefault 0

topSort :: [(Int, Int)] -> Mp.Map Int Int
topSort ls = 
    let topOrder = Mp.fromListWith max (map getOrder ls)
        getOrder (x, y) = (x, findWith0 y topOrder + 1)
     in topOrder

correcting rules book = 
    let goodRules = filter ((`elem` book) . fst) rules
        pagePower = topSort goodRules
     in sortOn (flip findWith0 pagePower) book

isConform ruleSet book = 
    let pairSet = S.fromList $ pairs (reverse book)
     in S.null $ S.intersection pairSet ruleSet

getMiddle l = l !! (length l `div` 2)

task1 rules books = 
    let ruleSet = S.fromList rules
        satisfied = filter (isConform ruleSet) books
     in sum $ map getMiddle satisfied

task2 rules books =
    let ruleSet   = S.fromList rules
        dissatisfied = filter (not . isConform ruleSet) books
     in sum $ map (getMiddle . correcting rules) dissatisfied

main = do
    rules <- map readRules <$> getAllLine
    books <- map (readIntsWith ",") <$> getAllLine
    print $ task1 rules books
    print $ task2 rules books
    


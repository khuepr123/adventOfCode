import Data.List
import Data.List.Split
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)) )
import Data.Maybe (mapMaybe)



getAllLine :: IO [String]
getAllLine = do
    line <- getLine
    if null line then return []
                 else fmap (line : ) getAllLine

processLine :: String -> (Int, NonEmpty Int)
processLine l = let [res, nums] = splitOn ":" l
                 in (read res, NE.fromList . map read . words $ nums)

hasSolution :: Int -> NonEmpty Int -> Maybe Int
hasSolution target nums
  | target `elem` allEquation nums = Just target
  | otherwise = Nothing

allEquation :: NonEmpty Int -> [Int]
allEquation l = foldl makeRes [NE.head l] (NE.tail l)

makeRes :: [Int] -> Int -> [Int]
makeRes ls num = map (+num) ls ++ map (*num) ls

task1 :: [(Int, NonEmpty Int)] -> Int
task1 = sum . mapMaybe (uncurry hasSolution)

-- allSplitEquation :: NonEmpty Int -> [Int]
-- allSplitEquation l = genSplit (NE.head l :| []) (NE.tail l)
--     where genSplit :: NonEmpty Int -> [Int] -> [Int]
--           genSplit nums resList = 
--               case NE.nonEmpty resList of
--                 Nothing -> allEquation nums
--                 Just trails@(h :| t) -> bonus ++ genSplit (NE.appendList nums [h]) t
--                     where bonus = liftA2 joinNumAsString (allEquation nums) (allSplitEquation trails)


allSplitEquation :: NonEmpty Int -> [Int]
allSplitEquation l = foldl makeResSplit [NE.head l] (NE.tail l)

makeResSplit :: [Int] -> Int -> [Int]
makeResSplit ls num = [(`joinNumAsString` num), (*num), (+num)] <*> ls


joinNumAsString :: Int -> Int -> Int
joinNumAsString a b = read (show a ++ show b)

task2 :: [(Int, NonEmpty Int)] -> Int
task2 = sum . mapMaybe (uncurry hasSplitSolution)

hasSplitSolution :: Int -> NonEmpty Int -> Maybe Int
hasSplitSolution target nums
  | target `elem` allSplitEquation nums = Just target
  | otherwise = Nothing

main = do
    l <- map processLine <$> getAllLine
    print $ task1 l
    print $ task2 l


    


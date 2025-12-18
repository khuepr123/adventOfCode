import qualified Data.Map as M
import Data.Maybe(fromMaybe)
import Control.Monad
import Data.List.Split


getAllLine :: IO [String]
getAllLine = do
    line <- getLine
    if null line then return []
                 else fmap (line : ) getAllLine

getGift = do
    l <- tail <$> getAllLine
    return $ length . filter (=='#') $ concat l

processLine :: String -> ((Int, Int), [Int])
processLine s = let src:dsts = words s
                    [x, y]   = map read $ splitOn "x" (init src)
                 in ((x, y), map read dsts)

stupidCheck lGift ((x, y), l) =
    sum l <= (x `div` 3) * (y `div` 3)

main = do
    l1 <- replicateM 6 getGift
    l2 <- map processLine <$> getAllLine
    let l3 = map (stupidCheck l1) l2
    print l3
    print $ sum . map fromEnum $ l3
 
                



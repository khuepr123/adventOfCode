

{-# LANGUAGE BangPatterns, TupleSections #-}
-- import qualified Data.Vector as V
import Data.Int
import Data.Array
import Data.List
import Data.Foldable
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe(fromJust)

readInt :: BS.ByteString -> Int32
readInt = fst . fromJust . BS.readInt

getInts :: IO [Int32]
getInts = map readInt . BS.words <$> BS.getLine

printYesNo :: Bool -> IO ()
printYesNo b = putStrLn (if b then "YES" else "NO")
 
-- readInts :: IO [Int32]
-- readInts = map read . words <$> getLine
 
vibecheck :: Bool -> IO () -> IO ()
vibecheck b m = if b then m else print (-1)
 
printArray :: Show a => [a] -> IO ()
printArray = putStrLn . unwords . map show 

printArrayList :: Show a => [[a]] -> IO ()
printArrayList = putStrLn . unlines . map (unwords . map show)

processLine :: IO [Int32]
processLine = do
    s <- getLine
    return $ elems $ accumArray (+) 0 ('A', 'Z') (map (,1) s)

inf = 10 ^ 14

calcBest :: Int32 -> [Int32] -> [Int32] -> Int32
calcBest m total chi = max (-1) $ m - maximum (zipWith minRepair total chi)
    where minRepair total part
            | part == 0 = 0
            | total == part = inf
            | otherwise = (total - 1) `quot` (total - part)

main = do
    [n, m] <- getInts
    l <- replicateM n processLine
    let total = foldr1 (zipWith (+)) l
        res = map (calcBest m total) l
    printArray res


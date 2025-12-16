import qualified Data.Map as M
import Data.Maybe(fromMaybe)


getAllLine :: IO [String]
getAllLine = do
    line <- getLine
    if null line then return []
                 else fmap (line : ) getAllLine

processLine :: String -> (String, [String])
processLine s = let src:dsts = words s
                 in (init src, dsts)

countPathToOut :: [(String, [String])] -> String -> String -> Int
countPathToOut l destination = 
    let edges = concatMap (\(x, y) -> map (x,) y) l
        f s   = fromMaybe 0 $ M.lookup s mp
        mp    = M.fromListWith (+) ((destination, 1) : map (\(x, y) -> (x, f y)) edges)
     in f
 
main = do
    l <- map processLine <$> getAllLine
    let destSrc = countPathToOut l
        calcPath trail = product $ zipWith destSrc (tail trail) trail
        path1 = calcPath ["svr", "dac", "fft", "out"]
        path2 = calcPath ["svr", "fft", "dac", "out"]
    print $ path1 + path2
                



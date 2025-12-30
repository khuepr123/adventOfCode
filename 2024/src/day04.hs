import Data.List

getAllLine :: IO [String]
getAllLine = do
    line <- getLine
    if null line then return []
                 else fmap (line : ) getAllLine

processLine :: String -> [Char]
processLine = id

rotate90 :: [[Char]] -> [[Char]]
rotate90 = reverse . transpose

-- 1
-- 32
-- 4

-- 12
-- 34

-- 12?
-- ?34

-- ?4
-- 23
-- 1?

rotate45 :: [[Char]] -> [[Char]]
rotate45 board = 
    let staircase = [replicate n '?' | n <- [0..length board-1]]
        shifted = zipWith3 connect3 staircase board (reverse staircase)
        newBoard = rotate90 . rotate90 . rotate90 $ shifted
     in map (filter (/='?')) newBoard
    where connect3 x y z = x ++ y ++ z



countPattern subPattern board =
    let chunks = allSubPattern (length subPattern) board
        in sum $ map (countSliced subPattern) chunks
       where countSliced subPattern chunk =
                let d1Pattern = map RegexDot $ transpose subPattern
                    d1Chunk   = map RegexDot $ transpose chunk
                 in countSubPattern d1Pattern d1Chunk

newtype RegexDot = RegexDot String

instance Eq RegexDot where
    RegexDot x == RegexDot y = and $ zipWith testDot x y
        where testDot x y = x == '.' || y == '.' || x == y

count :: Eq a => a -> [a] -> Int
count val = length . filter (val==)

allSubPattern :: Int -> [a] -> [[a]]
allSubPattern sz s = takeWhile ((sz==) . length) . map (take sz) $ iterate (drop 1) s

countSubPattern :: Eq a => [a] -> [a] -> Int
countSubPattern substr s = 
    let sections = allSubPattern (length substr) s
     in count substr sections


printArrayList :: [String] -> IO ()
printArrayList = mapM_ (putStrLn . unwords . map show)

countAll1d board = 
    let board90s = take 4 $ iterate rotate90 board
        board45s = map rotate45 board90s
        allBoard = board90s ++ board45s
        allSequences = concat allBoard
     in sum $ map (countSubPattern "XMAS") allSequences
   

xmasDiag = ["M.S"
           ,".A."
           ,"M.S"]

countAll2d board =
    let board90s = take 4 $ iterate rotate90 board
     in sum $ map (countPattern xmasDiag) board90s

main = do
    l <- getAllLine
    print $ countAll1d l
    print $ countAll2d l
    


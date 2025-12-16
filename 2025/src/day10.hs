-- get masks
-- perform xoring-fold
-- return result

import Data.Foldable as F
import Data.List as L
import Data.Ratio
import Data.Bits
import Data.List.Split
import Data.Maybe(fromJust, mapMaybe)
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Set as S
import Debug.Trace

getAllLine :: IO [String]
getAllLine = do
    line <- getLine
    if null line then return []
                 else fmap (line : ) getAllLine

processLine :: String -> ([Int], [[Int]], [Int])
processLine s = let cells = words s
                    getBy c = filter (\s -> c == head s) cells
                    [startString] = getBy '['
                    midStrings = getBy '('
                    [endString] = getBy '{'
                 in (fromImage startString, map toInts midStrings, toInts endString)
                where toInts = map read . splitOn "," . tail . init
                      fromImage = map fst . filter (\x -> snd x == '#') . zip [0..] . tail . init
                      

toMask :: [Int] -> Int
toMask = sum . map (2^)

instance {-# OVERLAPPING #-} Show (Ratio Int) where
    show x = if denominator x == 1
                then show (numerator x)
                else show (numerator x) ++ " % " ++ show (denominator x)

type Equation = V.Vector (Ratio Int)
type Reductor = (Int, Equation)

(<+>) = V.zipWith (+)

scaleUp :: Ratio Int -> Equation -> Equation
scaleUp r = V.map (*r)

eqFromList :: (Foldable t) => t Int -> Equation
eqFromList = V.fromList . map fromIntegral . F.toList

getEquationList :: [[Int]] -> [Int] -> [Equation]
getEquationList positionLists requirements =
    let taggedPositionLists = L.zip [1..] positionLists
        zeroVec = V.replicate (L.length positionLists + 1) 0
        taggedCoefs = L.zip [1..] requirements
        getEquation (position, theirSum) = 
            let goodCandidate = filter (L.elem position . snd) taggedPositionLists
             in eqFromList $ zeroVec V.// ((0, -theirSum) : map (\(x, _) -> (x, 1)) goodCandidate)
     in map getEquation taggedCoefs

nonzeros :: Equation -> [Int]
nonzeros eq = filter (\x -> eq V.! x /= 0) [1..V.length eq - 1]

reduce :: Reductor -> Equation -> Equation
reduce (pos, eq1) eq2 = scaleUp coef eq1 <+> eq2 
    where coef = - (eq2 V.! pos) / (eq1 V.! pos)

reduceMeta :: Reductor -> Reductor -> Reductor
reduceMeta red1@(pos1, eq1) red2@(pos2, _)
  | pos1 == pos2 = red1
  | otherwise    = (pos1, reduce red2 eq1)

normalize :: Reductor -> Reductor
normalize (pos1, eq1) = (pos1, scaleUp (-1 / (eq1 V.! pos1)) eq1)

calcFrom :: Equation -> [Int] -> Maybe Int
calcFrom eq ls = 
    let arg = sum $ V.zipWith (*) eq (eqFromList $ 1:ls)
     in case (numerator arg, denominator arg) of
          (x, 1) | x >= 0 -> Just x
          _               -> Nothing

getBestSol :: A.Array Int Int -> [Int] -> [Reductor] -> Int
getBestSol ceiling freeVars retrievers =
    let (1, n) = A.bounds ceiling
        modifiers = map (\x -> map (x,) [0..ceiling A.! x]) freeVars
        modifiers :: [[(Int, Int)]]
        augMods = foldr (liftA2 (:)) [[]] modifiers
        baseArray = A.listArray (1, n) (replicate n 0)
        candidates = map (F.toList . (baseArray A.//) ) augMods
     in minimum . mapMaybe (evaluate retrievers) $ candidates

evaluate :: [Reductor] -> [Int] -> Maybe Int
evaluate retrievers candidate =
    let suitors = map (flip calcFrom candidate . snd) retrievers
     in (sum candidate + ) <$> foldl (liftA2 (+)) (Just 0) suitors
    

createCeiling indexLists reqs = A.listArray (1, length indexLists) $ L.map (minimum . L.map (reqs !!)) indexLists

calculateMinJoltagePress :: (a, [[Int]], [Int]) -> Int
calculateMinJoltagePress (_, zeroIndexPositionLists, requirements) = 
    let positionLists = map (map (+1)) zeroIndexPositionLists
        argCount = L.length positionLists
        equations = getEquationList positionLists requirements
        reductors = reducing equations
        retrievers = map (\x -> foldr (flip reduceMeta) x reductors) reductors
        ceilings :: A.Array Int Int
        ceilings = createCeiling zeroIndexPositionLists requirements -- TODO: change to max
        freeVars = F.toList $ S.fromList [1..argCount] S.\\ S.fromList (map fst reductors)
     in getBestSol ceilings freeVars (map normalize retrievers)
                
    where reducing :: [Equation] -> [Reductor]
          reducing = reduceAction []
              where reduceAction reds oldEqs = 
                        let eqs = map (\x -> foldr reduce x reds) oldEqs
                         in case eqs of
                              []        -> reds
                              hEq : tEq -> case nonzeros hEq of
                                             []          -> reduceAction reds tEq
                                             randId : _  -> reduceAction ((randId, hEq) : reds) tEq
                  
-- (0) (0) {1}

-- getBestSol :: A.Array Int Int -> [Int] -> [Reductor] -> Int

main = do
    parts <- map processLine <$> getAllLine
    let results = map calculateMinJoltagePress parts
    print results
    print $ sum results

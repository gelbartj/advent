import qualified Data.Text as T
import Data.Char ( isDigit )
import Data.List ( minimumBy, foldl' )
import Data.Ord ( comparing )
import Data.Maybe ( fromJust, isJust )

earliest :: Integer
earliest = 1000186

inputList = "17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,907,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,29,x,653,x,x,x,x,x,x,x,x,x,41,x,x,13"

sched :: [T.Text]
sched = T.splitOn (T.pack ",") (T.pack inputList)
schedI :: [Integer] -- For part 1
schedI = map read $ filter (all isDigit) $ map T.unpack sched :: [Integer]

newtype Gap = Gap Integer deriving (Eq, Show)
newtype BusNum = BusNum Integer deriving (Eq, Show)

makeSchedGaps :: [T.Text] -> [(BusNum, Gap)]
makeSchedGaps sched' = map (\(b,a) -> (BusNum (read (T.unpack b) :: Integer), Gap a)) (
    filter (\i -> fst i /= T.pack "x") (zip sched' [0..])
    )

schedGaps :: [(BusNum, Gap)]
schedGaps = makeSchedGaps sched

-- List of tuple pairs with infinite lists
allBuses :: [Integer] -> [(Integer, [Integer])]
allBuses = map (\i -> (i, iterate (+i) 0))

-- For part 1
availBuses :: [Integer] -> [(Integer, Integer)] -- (bus id, first bus)
availBuses schedI' = map (\i -> (fst i, (head . dropWhile (< earliest) . snd) i))
    (allBuses schedI')

-- Part 2
-- This code does work, but it takes a very long time to compute...
-- Used WolframAlpha instead to get the answer
busFitsGap :: BusNum -> Integer -> Bool
busFitsGap b@(BusNum bus) leadTimestamp = let gap = lookup b schedGaps in
    (leadTimestamp + (\(Gap g) -> g) (fromJust gap)) `mod` bus == 0

startTime :: Integer
startTime = 100000000000000

matchingTime :: Integer
matchingTime = head $ dropWhile (not . allMatch) (iterate (+firstBus) startTime) where
    firstBus :: Integer
    firstBus = (\(BusNum x) -> x) $ fst (head schedGaps)
    allMatch leadTimestamp = all ((`busFitsGap` leadTimestamp) . fst) schedGaps

main :: IO ()
main = do
    -- Part 1
    -- let firstBus = minimumBy (comparing snd) (availBuses schedI)
    -- prInteger firstBus
    -- prInteger $ (snd firstBus - earliest) * fst firstBus
    print schedGaps


splitOnPredicate :: (Char -> Bool) -> String -> [String]
splitOnPredicate _ [] = []
splitOnPredicate p (x':xs') = split xs' [[x']] where
    split [] acc = acc
    split (x:xs) acc
        | p x = split xs (init acc ++ [last acc ++ [x]])
        | otherwise = split xs (acc ++ [[x]])
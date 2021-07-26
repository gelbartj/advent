import qualified Data.Text as T
import Data.Char ( isDigit )
import Data.List ( minimumBy )
import Data.Ord ( comparing )

earliest :: Int
earliest = 1000186
sched :: [T.Text]
sched = T.splitOn (T.pack ",") (T.pack "17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,907,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,x,x,x,x,x,x,x,23,x,x,x,x,x,29,x,653,x,x,x,x,x,x,x,x,x,41,x,x,13")
schedI :: [Int]
schedI = map read $ filter (all isDigit) $ map T.unpack sched :: [Int]

-- List of tuple pairs with infinite lists
allBuses :: [(Int, [Int])]
allBuses = map (\i -> (i, iterate (+i) 0)) schedI

availBuses :: [(Int, Int)] -- (bus id, first bus)
availBuses = map (\i -> (fst i, (head . dropWhile (< earliest) . snd) i)) allBuses

main = do
    let firstBus = minimumBy (comparing snd) availBuses
    print firstBus
    print $ (snd firstBus - earliest) * fst firstBus
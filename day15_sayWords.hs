import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )
import Debug.Trace ( trace )
import Data.Function ( (&) )
import Data.List ( foldl' ) 

startList :: [Int]
startList = [1,0,18,10,19,6]

startMap :: Map.Map Int (Int,Int)
startMap = Map.fromList (zip startList (zip (repeat 0) [1..]))

steps :: Map.Map Int (Int,Int) -> Int -> Int -> [(Int, Map.Map Int (Int, Int))]
steps numsMap prevNum stepNum = singleStep numsMap prevNum stepNum & 
    (\(newNum, newMap) -> (newNum, newMap):steps newMap newNum (succ stepNum))

singleStep :: Map.Map Int (Int, Int) -> Int -> Int -> (Int, Map.Map Int (Int, Int))
singleStep numsMap prevNum stepNum = let
    prevPositions = Map.lookup prevNum numsMap
    newNum = case prevPositions of
        Nothing -> 0
        Just (0, _) -> 0
        Just (a, b) -> b - a
    rotate (_, b) c = (b, c)
    newMap = Map.alter
        (\currVal -> Just (rotate (fromMaybe (0,0) currVal) stepNum)) newNum numsMap
    in (newNum, newMap)

reduceStep numSteps numsMap prevNum stepNum = 
    fst $ foldl' (\(newNum, newMap) stepNum' -> singleStep newMap newNum stepNum') 
        (prevNum, numsMap) [stepNum..stepNum + numSteps]

main :: IO ()
main = do
    putStr "Result: "
    let finalIdx = 30000000
    let numChunks = 6
    let chunkSize = finalIdx `div` numChunks
    let (num1, map1) = last $ take (chunkSize - length startList) $ steps startMap (last startList) (length startList + 1)
    
    -- Iteration will only work if finalIdx is divisible by numChunks
    let finalNum = last $ map (fst . snd) $ take numChunks $ 
            iterate (\(loopNum, (numX, mapX)) -> 
                (loopNum + 1, last $ take chunkSize $ 
                    steps mapX numX (loopNum * chunkSize + 1)))
                (1, (num1, map1))
    print finalNum

    let origAnswer = last $ map fst $ take (finalIdx - length startList) $ steps startMap (last startList) (length startList + 1)
    putStrLn $ "Correct (original impl.): " ++ show origAnswer    
    
    -- This one ended up being super inefficient!
    --print $ reduceStep (2020 - length startList - 1) startMap (last startList) (length startList + 1)
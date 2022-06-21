-- import Data.List ( permutations, sort )
import Data.Maybe (fromMaybe)
import Data.Array ( (!), (//), Array, listArray )
import Control.Monad ( forM )

adapters :: [Int]
adapters = [0, 1, 2, 3, 4, 7, 8, 9, 10, 13, 14, 15, 16, 17, 20, 23, 24, 25, 26, 27, 30, 31, 32, 33, 34, 37, 38, 39, 40, 41, 44, 45, 46, 47, 48, 51, 52, 53, 54, 55, 58, 59, 60, 63, 64, 65, 66, 69, 70, 71, 72, 75, 76, 77, 80, 83, 84, 85, 86, 87, 90, 91, 94, 95, 96, 97, 98, 101, 104, 105, 108, 109, 110, 111, 112, 115, 116, 117, 118, 121, 124, 127, 128, 131, 134, 135, 138, 139, 140, 143, 144, 145, 148, 149, 150, 151, 152, 155, 156, 157, 160]
    -- sort [151,94,14,118,25,143,33,23,80,95,87,44,150,39,148,51,138,121,70,69,90,155,144,40,77,8,97,45,152,58,65,63,128,101,31,112,140,86,30,55,104,135,115,16,26,60,96,85,84,48,4,131,54,52,139,76,91,46,15,17,37,156,134,98,83,111,72,34,7,108,149,116,32,110,47,157,75,13,10,145,1,127,41,53,2,3,117,71,109,105,64,27,38,59,24,20,124,9,66, 0, 160]

makeTree :: (Ord a, Num a) => [a] -> [(a, [a])]
makeTree adapterList = map (\ node -> (node, filter (\x -> x - node <= 3 && x - node > 0) adapterList)) adapterList

makeReverseTree :: (Ord a, Num a) => [a] -> [(a, [a])]
makeReverseTree adapterList = map (\ node -> (node, filter (\x -> node - x <= 3 && node - x > 0) adapterList)) adapterList

numPaths :: Int -> [(Int, [Int])] -> Int
numPaths start tree = length options + sum (map (`numPaths` tree) options) where
    options = fromMaybe [] (lookup start tree)

{- adapterCount :: [Int] -> Int -> Int
adapterCount [] _ = 0
adapterCount adapters' currJolt = 1 + sum (map (\item -> adapterCount (filter (/=item) adapters') item) options) where
    options = filter (\item -> (item - currJolt <= 3) && (item - currJolt > 0)) adapters'
 -}

dfs :: Int -> Int -> [(Int, [Int])] -> Array Int Int
dfs start dest graph = dfs' start dest [] (listArray (0, maximum adapters) (repeat 0)) graph where
    dfs' :: Int -> Int -> [Int] -> Array Int Int -> [(Int, [Int])] -> Array Int Int
    dfs' start' dest' visited pathCounts graph' = do
        let newVisited = start':visited
        --let newPath = path ++ [start']
        let newPathCounts = pathCounts // [(start', (pathCounts ! start') + (if start' == dest' then 1 else 0))]
        let neighbors = fromMaybe [] (lookup start' graph')
        let recursedCounts = if start' /= dest' then 
                             foldr (\neighbor pathAcc -> dfs' neighbor dest' newVisited pathAcc graph') 
                                newPathCounts (filter (`notElem` newVisited) neighbors) 
                             else newPathCounts
        recursedCounts

-- ## PART 2 ## --

{- List of adapters with only 1 way to get to or from them, so their paths are independent and counts can be 
multiplied together. Created using a table made in Excel. -}
loners :: [Int]
loners = [0,7,13,20,23,30,37,44,51,58,63,69,75,80,83,90,91,94,101,104,105,108,115,121,124,127,128,131,134,135,138,143,148,155,160]

pathCount :: Int -> Int -> [(Int, [Int])] -> Int
pathCount start end tree = (dfs start end tree) ! end

calcLengths :: [Int] -> [(Int, [Int])] -> Int
calcLengths loneList tree = calc' 1 loneList tree where
    calc' _ [] _ = 0
    calc' _ _ [] = 0
    calc' initVal (x:xs) tree'
        | null xs = initVal
        | otherwise = calc' (initVal * (pathCount x (head xs) tree')) xs tree'

main :: IO ()
main = do
    let tree = makeTree adapters
    print $ calcLengths loners tree
    
    --print $ (dfs (head intArgs) (last intArgs) (makeTree adapters)) ! (last intArgs)
import Data.List ( permutations, sort )
import Data.Maybe (fromMaybe)
import Data.Array ( (!), (//), array, Array, listArray )
import Control.Monad ( forM )

adapters :: [Int]
adapters = sort [151,94,14,118,25,143,33,23,80,95,87,44,150,39,148,51,138,121,70,69,90,155,144,40,77,8,97,45,152,58,65,63,128,101,31,112,140,86,30,55,104,135,115,16,26,60,96,85,84,48,4,131,54,52,139,76,91,46,15,17,37,156,134,98,83,111,72,34,7,108,149,116,32,110,47,157,75,13,10,145,1,127,41,53,2,3,117,71,109,105,64,27,38,59,24,20,124,9,66, 0, 160]

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

dfs start dest graph = dfs' start dest [] (listArray (0,160) (repeat 0)) graph where
    dfs' :: Int -> Int -> [Int] -> Array Int Int -> [(Int, [Int])] -> Array Int Int
    dfs' start dest visited pathCounts graph = do
        let newVisited = start:visited
        --let newPath = path ++ [start]
        let newPathCounts = pathCounts // [(start, if start == dest then 1+(pathCounts ! start) else pathCounts ! start)]
        let neighbors = fromMaybe [] (lookup start graph)
        let recursedCounts = if start /= dest then foldr (\neighbor pathAcc -> dfs' neighbor dest newVisited pathAcc graph) newPathCounts (filter (`notElem` newVisited) neighbors) else newPathCounts
        recursedCounts



main = do
    print $ dfs 157 160 (makeTree adapters)
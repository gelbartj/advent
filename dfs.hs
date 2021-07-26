import Data.List ( sort )
import Data.Maybe ( fromMaybe )

adapters :: [Int]
adapters = sort [151,94,14,118,25,143,33,23,80,95,87,44,150,39,148,51,138,121,70,69,90,155,144,40,77,8,97,45,152,58,65,63,128,101,31,112,140,86,30,55,104,135,115,16,26,60,96,85,84,48,4,131,54,52,139,76,91,46,15,17,37,156,134,98,83,111,72,34,7,108,149,116,32,110,47,157,75,13,10,145,1,127,41,53,2,3,117,71,109,105,64,27,38,59,24,20,124,9,66, 0, 160]

makeTree :: (Ord a, Num a) => [a] -> [(a, [a])]
makeTree adapterList = map (\ node -> (node, filter (\x -> x - node <= 3 && x - node > 0) adapterList)) adapterList

dfs current visited = let graph = makeTree adapters in
    foldl (\visited next -> if next `elem` visited then visited else dfs next visited)
           (visited ++ [current]) (fromMaybe [] (lookup current graph))


import Data.Bifunctor (second)
import Data.List (nub)

inputString :: String
inputString = "..##.##.\n#.#..###\n##.#.#.#\n#.#.##.#\n###..#..\n.#.#..##\n#.##.###\n#.#..##."

rows :: [(Integer, String)]
rows = zip [0..] (lines inputString)
activeCols :: [(Integer, [(Integer, Char)])]
activeCols = map (second (filter (\ x -> snd x == '#') . zip [0 .. ])) rows
activeStartCoords :: [(Integer, Integer, Integer)]
activeStartCoords = concatMap (\(rowNum, colTuples) ->
    map (\(colIdx, _) -> (rowNum, colIdx, 0)) colTuples) activeCols

neighbors :: (Num c, Enum c, Eq c) => (c, c, c) -> [(c, c, c)]
neighbors (inputX, inputY, inputZ) = [(inputX+dx, inputY+dy, inputZ+dz) |
    dx <- [-1..1], dy <- [-1..1], dz <- [-1..1],
    any (/=0) [dx,dy,dz]]

neighbors4d :: (Num c, Enum c, Eq c) => (c, c, c, c) -> [(c, c, c, c)]
neighbors4d (inputX, inputY, inputZ, inputW) = [(inputX+dx, inputY+dy, inputZ+dz, inputW+dw) |
    dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], dw <- [-1..1],
    any (/=0) [dx,dy,dz,dw]]

nextGen :: Eq a => [a] -> (a -> [a]) -> [a]
nextGen activeList neighborsFun = let allCoords = nub $ activeList ++ concatMap neighborsFun activeList
                                      inactiveList = filter (`notElem` activeList) allCoords
                                      activeNeighborCount = (length . 
                                        filter (`elem` activeList) . neighborsFun) in
                                      filter (\x -> activeNeighborCount x `elem` [2,3]) activeList
                                        ++ filter (\x -> activeNeighborCount x == 3) inactiveList
         
iterateGen :: Eq a => Int -> [a] -> (a -> [a]) -> [a]
iterateGen 0 activeList _ = activeList
iterateGen numGens activeList neighborsFun = nextGen (iterateGen (numGens - 1) activeList neighborsFun) neighborsFun

main = do
    -- Part 1
    print $ length $ iterateGen 6 activeStartCoords neighbors

    -- Part 2
    let startCoords4d = map (\(x,y,z) -> (x,y,z,0)) activeStartCoords
    print $ length $ iterateGen 6 startCoords4d neighbors4d

import Data.List ( inits )

preambleLength = 25
secretNum = 258585477

isValid nums =
  currNum `elem` possibilities where
    currNum = nums !! preambleLength
    preamble = take preambleLength nums
    possibilities = [ a + b | a <- preamble, b <- preamble, a + b == currNum && a /= b ]

processIdxs nums = if isValid nums then processIdxs (drop 1 nums) else nums !! preambleLength

partialSums :: [Int] -> Int
partialSums [] = 0
partialSums origNums = if part /= 0 then part else partialSums (drop 1 origNums) where
    part = foldr (\item acc -> if acc > 0 then acc else
       (if not (null item) && head item < secretNum && sum item == secretNum then maximum item + minimum item else 0)) 0
            (inits origNums)


main = do
  contents <- readFile "nums.txt"
  let nums = map read $ lines contents :: [Int]
  print $ partialSums nums
import Data.List

preambleLength = 25
secretNum = 258585477

isValid nums = 
  currNum `elem` possibilities where
    currNum = nums !! preambleLength
    preamble = take preambleLength nums
    possibilities = [ a + b | a <- preamble, b <- preamble, (a + b == currNum && a /= b) ]

processIdxs nums = if isValid nums then processIdxs (drop 1 nums) else (nums !! preambleLength)

partialSums [] = []
partialSums origNums = filter (>0) (map (\item -> if sum item == secretNum then (maximum item + minimum item) else 0) (inits origNums) ++ partialSums (drop 1 origNums))

main = do
  contents <- readFile "nums.txt"
  let nums = map read $ lines contents :: [Int]
  print $ partialSums nums
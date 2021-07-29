import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import Data.Bits ( Bits((.&.), shift, complement, (.|.)) )
import qualified Data.Map as Map
import Control.Applicative ( Applicative(liftA2) )

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + (if x == 'X' then 0 else digitToInt x)) 0

xPositions :: String -> Char -> [Int]
xPositions binString keepChar = map fst $
    filter (\i -> snd i == keepChar) (zip [shift 1 n | n <- [0..]] (reverse binString))

maskFilledBlanks :: String -> Int -> Char -> Int
maskFilledBlanks maskString num keepChar = (num .&. sum xs) +
    toDec maskString where xs = xPositions maskString keepChar

data Part = Part1 | Part2 deriving Eq

processLine :: Part -> (Map.Map Int Int, String) -> String -> (Map.Map Int Int, String)
processLine isPart1 (memMap, currMask) str = case take 3 str of
    "mas" -> (memMap, val)
    "mem" -> (if isPart1 == Part1 then memProcess1 else memProcess2, currMask)
    _ -> (memMap, currMask)
    where val = last $ words str
          memAddr = read (takeWhile isDigit (dropWhile (not . isDigit) str)) :: Int
          memProcess1 = Map.insert memAddr (maskFilledBlanks currMask (read val :: Int) 'X')
            memMap
          memProcess2 = foldr (\item acc -> Map.insert item (read val :: Int) acc)
            memMap (processMemAddr currMask memAddr)

-- For part 2
-- Returns list of list of all possible amounts that need to be added
floating :: String -> [Int]
floating maskString = map sum $ go $ map (:[0]) (xPositions maskString 'X')
    where
    go [] = [[]]
    go (num:nums) = liftA2 (:) num (go nums)

processMemAddr :: String -> Int -> [Int]
processMemAddr mask memAddr = map (interim +) (0:floating mask) where
    floatMask = complement $ sum (xPositions mask 'X')
    interim = maskFilledBlanks mask memAddr '0' .&. floatMask

main :: IO ()
main = do
  contents <- readFile "bitmask.txt"
  let contentLines = lines contents
  -- Part 1
  print $ sum $ Map.elems $ fst $ foldl' (processLine Part1) (Map.empty, "") contentLines
  -- Part 2
  print $ sum $ Map.elems $ fst $ foldl' (processLine Part2) (Map.empty, "") contentLines
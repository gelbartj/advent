module Main where

import Data.Array
import Text.Read
import Data.Maybe (fromMaybe)

processInstructions :: Array Int String -> Maybe Int
processInstructions instructions = process ([], 0, 0) where
  instructionCount = length instructions
  process (executed, acc, instructionLine)= case op of
    "nop" -> if instructionLine + 1 >= instructionCount then Just acc else process (executed ++ [instructionLine], acc, instructionLine + 1)
    "acc" -> if instructionLine + 1 >= instructionCount then Just (acc + amt) else process (executed ++ [instructionLine], acc + amt, instructionLine + 1)
    "jmp" -> if (instructionLine + amt) `elem` executed then Nothing else
        if instructionLine + amt >= instructionCount then Just acc else
          process (executed ++ [instructionLine], acc, instructionLine + amt)
    _ -> Nothing
    where
        instWords = words $ instructions ! instructionLine
        op = head instWords
        amt :: Int
        amt = fromMaybe 0 (readMaybe (filter (/='+') (last instWords)))

changeInstruction :: Int -> Array Int String -> Array Int String
changeInstruction startLine instructions
  | head instruction == "acc" = instructions
  | head instruction == "nop" = instructions // [(startLine,  "jmp " ++ unwords (tail instruction))]
  | head instruction == "jmp" = instructions // [(startLine,  "nop " ++ unwords (tail instruction))]
  | otherwise = instructions
  where instruction = words $ instructions ! startLine

processIterative :: Array Int String -> Int
processIterative arr = go arr 0 where
    go arr' startLine = case processInstructions arr' of
        Nothing -> go (changeInstruction (startLine + 1) arr) (startLine + 1)
        Just x -> x

main = do
  contents <- readFile "instructions.txt"
  let contentLines = lines contents
  let contentArray = array (0, length contentLines - 1) (zip [0..] contentLines)
  print $ processIterative contentArray
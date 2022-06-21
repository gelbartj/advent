import qualified Data.Text as T
import Data.Char (isDigit)
import Debug.Trace (trace)

-- Using string instead of char for numbers allows for multi-digit nums
-- Using `read` also automatically ignores whitespace
evaluate :: String -> Char -> String -> Int
evaluate num1 op num2 = let
    parsedNum1 = read num1 :: Int
    parsedNum2 = read num2 :: Int in
    case op of
        '+' -> parsedNum1 + parsedNum2
        '*' -> parsedNum1 * parsedNum2
        '-' -> parsedNum1 - parsedNum2  -- not used in puzzle input but might as well include
        _ -> 0

opList :: [Char]
opList = ['+', '*', '-']

parseLR :: String -> Int
parseLR [] = 0
parseLR origStr@(c:cs) = parse' c cs "" '_' "" where
    parse' :: Char -> String -> String -> Char -> String -> Int
    parse' currChar [] storedNum1 storedOp storedNum2 = 
        if storedOp == '_' then (read storedNum1 :: Int) else
        evaluate storedNum1 storedOp (storedNum2 ++ (if isDigit currChar then (currChar:[]) else []))
    parse' currChar remStr@(c':cs') storedNum1 storedOp storedNum2
        | currChar == ' ' = if ((not . null) storedNum1 && (not . null) storedNum2 && storedOp /= '_') then
            let result = show $ evaluate storedNum1 storedOp storedNum2 in
                parse' c' cs' result '_' "" 
            else parse' c' cs' storedNum1 storedOp storedNum2
        | currChar `elem` opList = parse' c' cs' storedNum1 currChar storedNum2
        | isDigit currChar  = if storedOp == '_' then 
           parse' c' cs' (storedNum1 ++ currChar:[]) storedOp storedNum2
            else parse' c' cs' storedNum1 storedOp (storedNum2 ++ currChar:[])
        | currChar == '(' = let (parsedRem, rem') = parseParens remStr in
            -- trace ("parseParens \"" ++ remStr ++ "\" = " ++ (show $ (parseParens remStr)))
            if null rem' then
                if storedOp == '_' then parsedRem else 
                    evaluate storedNum1 storedOp (show parsedRem)
            else
                if storedOp == '_' then
                    parse' (head rem') (tail rem') (show parsedRem) storedOp storedNum2
                    else parse' (head rem') (tail rem') storedNum1 storedOp (show parsedRem)
        | currChar == ')' =  evaluate storedNum1 storedOp storedNum2 -- remainder of eqn handled by recursive calls
        
        -- parse' c' cs' (show $ evaluate storedNum1 storedOp storedNum2) '_' ""

        {- trace ("While processing \"" ++ origStr ++ "\", encountered `)`.\n     Remaining string is \"" ++ remStr ++
            "\", storedNum1 is " ++ storedNum1 ++ ", storedOp is " ++ (storedOp:[]) ++ 
            ", storedNum2 is " ++ storedNum2) $ -}
        | otherwise = 0

    parseParens :: String -> (Int, String)
    -- Will always receive a string that is a parenthetical expression WITHOUT opening `(`
    -- Should return simplified result of expression and all nested parenthetical groups, with
    -- remainder of original string after parsing through final `)`
    parseParens [] = (0, [])
    parseParens s = (parseLR nestedParens, remStr) where
        remStr = go s 1 where
            go :: String -> Int -> String
            go s' 0 = s'
            go [] _ = [] -- shouldn't happen, means mismatched parens
            go (c':cs') level = case c' of
                '(' -> go cs' (level + 1)
                ')' -> go cs' (level - 1)
                _ -> go cs' level
        nestedParens = take (length s - length remStr) s

getAnswer :: (String, Int) -> IO ()
getAnswer (inString, answer) = do
    let parsedAns = parseLR inString
    print $ inString ++ " = " ++ (show parsedAns) ++ 
        if (parsedAns == answer) then "  -> Yes" else " -> NO, should be " ++ show answer

main = do
    contents <- readFile "math.txt"
    let problems = lines contents
    getAnswer ("4 + 5 + 2 * 6", 66)
    getAnswer ("4 + 5 + (2 * 6)", 21)
    getAnswer ("(4 + 5)", 9)
    getAnswer ("4 + 5 + (2 * 6) + 2", 23)
    getAnswer ("4 + 5 + ((2 * (6 + 3))", 27)
    getAnswer ("4 + 5 + ((2 * (6 + 3)) + 1", 28)
    getAnswer ("4 + 5 + ((2 * (6 + 3)) + (2 * 3)", 33)
    getAnswer ("4 + 5 + ((2 * (6 + 3)) + (2 * (2 + 3))", 37)
    getAnswer ("1 + 2 + 3 * 6 * 1 + 7", 43)
    getAnswer ("(1 + 2 + 3 * 6 * 1 + 7) * 3", 129)
    getAnswer ("1 + 2 + (3 * 6) * (1 + 7)", 168)
    getAnswer ("1 + 2 + (3 * 6) + (1 * 7)", 28)
    getAnswer ("2 * 3 + (4 * 5)", 26)
    getAnswer ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437)
    getAnswer ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240)
    getAnswer ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632)
    print $ sum $ (map parseLR problems)
    
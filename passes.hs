import System.IO ( hGetContents, withFile, IOMode(ReadMode) )
import Data.List.Split ( splitOn )
import Data.List ( sort, nub )
import Data.Char ( isDigit )
import Data.Maybe ( fromJust, isNothing, isJust )

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:
-- If cm, the number must be at least 150 and at most 193.
-- If in, the number must be at least 59 and at most 76.
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.
-- cid (Country ID) - ignored, missing or not.

getPasses :: [Char] -> [[Char]]
getPasses = splitOn "\n\n"

getFields :: String -> [String]
getFields s = map (takeWhile (/= ':')) (words s)

getFieldPairs :: String -> [(String, String)]
getFieldPairs s = map (\w -> let (field, val) = span (/= ':') w in
    (field, tail val)) (words s)

newtype Field = Field String deriving (Eq, Show, Ord)

verifyYr :: Field -> [Char] -> Bool
verifyYr field s = length s == 4 && all isDigit s && int >= startYr && int <= endYr where
    int = read s :: Int
    (startYr, endYr) = case field of
        Field "byr" -> (1920, 2002)
        Field "iyr" -> (2010, 2020)
        Field "eyr" -> (2020, 2030)
        _ -> (0,0)

verifyHgt :: [Char] -> Bool
verifyHgt s
    | null $ takeWhile isDigit s = False
    | otherwise = case dropWhile isDigit s of
        "cm" -> num >= 150 && num <= 193
        "in" -> num >= 59 && num <= 76
        _ -> False
        where
            num = read (takeWhile isDigit s) :: Int

verifyHcl :: [Char] -> Bool
verifyHcl s = head s == '#' && length rest == 6 && all
    (\s -> s `elem` ['0'..'9']++['a'..'f']) rest where
        rest = tail s

verifyEcl :: [Char] -> Bool
verifyEcl s = s `elem` ["amb","blu","brn","gry","grn","hzl","oth"]

verifyPid :: String -> Bool
verifyPid s = length s == 9 && all isDigit s

reqList :: [Field]
reqList = map Field ["byr","iyr","eyr","hgt","hcl","ecl","pid","cid"]

funcs :: [(Field, [Char] -> Bool)]
funcs = zip reqList [verifyYr (Field "byr"), verifyYr (Field "iyr"), verifyYr (Field "eyr"),
    verifyHgt, verifyHcl, verifyEcl, verifyPid, const True]

reqList2 :: [Field]
reqList2 = sort (Field "cid":reqList)

validateField :: ([Char], [Char]) -> (String, Bool)
validateField (field, val) =
    do
        -- print $ "Looking up: " ++ field ++ ", " ++ val
        let func = lookup (Field field) funcs
        -- if isNothing func then print "Nothing!" else print "Func found"
        let msg = if isNothing func then "Invalid function: " ++ field else field ++ " " ++ (if fromJust func val then "valid" else "invalid") ++ ": " ++ val
        (msg, isJust func && fromJust func val)


main :: IO ()
main = do
    withFile "passInfo.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let pairs = map getFieldPairs $ getPasses contents
        {- (
                (map Field (sort $ map fst passport) == reqList)
                || (map Field (sort $ map fst passport) ==
                    filter (/= Field "cid") reqList)
                ) -}
        -- print pairs
        print $ length $ filter id $ map (\passport ->
            all (snd . validateField) passport &&
            (let sorted = map Field (sort $ map fst passport) in
                sorted == sort reqList ||
                sorted == sort (init reqList)
            )) pairs
        --print $ map (map (fst. validateField)) pairs
        )
        {- print $ length $ filter id (map 
            (((\s -> (s == reqList) || (s == reqList2)) . sort) . getFields) 
                (getPasses contents))) -}
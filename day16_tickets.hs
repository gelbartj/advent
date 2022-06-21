import qualified Data.Text as T
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Data.Bifunctor ( second )

yourTicket :: [Integer]
yourTicket = [59,101,191,149,167,197,199,137,163,131,113,67,103,97,61,139,157,151,193,53]

checkValue :: [([Int], [Int])] -> Int -> Bool
checkValue rules val = any (\([lo1, hi1], [lo2, hi2]) -> (val >= lo1 && val <= hi1) || (val >= lo2 && val <= hi2)) rules

checkValueByRule :: [Int] -> (String, ([Int], [Int])) ->  Bool
checkValueByRule vals (ruleName, ([lo1, hi1], [lo2, hi2])) =
    all (\val -> (val >= lo1 && val <= hi1) || (val >= lo2 && val <= hi2)) vals
checkValueByRule _ _ = False

parseTickets :: T.Text -> [[Int]]
parseTickets ticketInput =  map (map (read . T.unpack) . T.splitOn (T.pack ",")) $ T.lines ticketInput

checkTicket :: [([Int], [Int])] -> [Int] -> [Int]
checkTicket rules = map (\val -> if checkValue rules val then 0 else val)

checkTicketBool :: [([Int], [Int])] -> [Int] -> Bool
checkTicketBool rules = all (checkValue rules)

makeCols :: [[a]] -> [[a]]
makeCols [] = []
makeCols tickets = map head tickets:rest where
    rest = if all (null . drop 1) tickets then [] else
        makeCols (map (drop 1) tickets)

parseRules :: T.Text -> [(String, ([Int], [Int]))]
parseRules rawRules = map ((\(ruleName, fullRule) -> let
    ruleWords = T.words fullRule
    dashSplit :: T.Text -> [Int]
    dashSplit = map (read . T.unpack) . T.splitOn (T.pack "-") in
    (T.unpack ruleName,
        (dashSplit $ ruleWords !! 1, dashSplit $ ruleWords !! 3))
    ) . T.break (==':')) (T.lines rawRules)

assignRules :: [(Int, [Int])] -> [(String, ([Int], [Int]))] -> [(Int, String)]
assignRules [] _ = []
assignRules _ [] = []
assignRules columnsWithIdx rules =
  let firstEl = second head (head $ filter (\x -> length (snd x) == 1)
       (map (\(idx, col) -> (idx, map fst $ filter (checkValueByRule col) rules)
        ) columnsWithIdx))
      colsRemaining = filter (\(idx, _) -> idx /= fst firstEl) columnsWithIdx
      rulesRemaining = filter (\(ruleName, _) -> ruleName /= snd firstEl) rules
       in firstEl:assignRules colsRemaining rulesRemaining

main :: IO ()
main = do
  contents <- readFile "tickets.txt"
  let [rawRules, rawTickets] = T.splitOn (T.pack "\n\n") (T.pack contents)
  let rules = parseRules rawRules
  let tickets = parseTickets rawTickets
  --print $ map (length. checkValueName rules') (tickets !! 0)
  let noNames = map snd rules
  print $ sum $ map (sum . checkTicket noNames) tickets

  -- Part 2
  let validTicketsOnly = filter (checkTicketBool noNames) tickets
  let columnsWithIdx = zip [0..] (makeCols validTicketsOnly)
  let labeledCols = assignRules columnsWithIdx rules
  let deptCols = filter (\(_, ruleName) -> take 9 ruleName == "departure") labeledCols
  print $ product $ map ((yourTicket !!) . fst) deptCols
  --print $ map (\(idx, col) -> (idx, map fst $ filter (checkValueByRule col) rules)) columnsWithIdx

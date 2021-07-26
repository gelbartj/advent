module Bags where


import qualified Data.Text as T
import Text.Read
import Control.Monad
import Data.List

processLine :: String -> (T.Text, [(Int, T.Text)])
processLine s = let [container, items] = T.splitOn (T.pack " bags contain ") (T.pack s)
  in (container, map ((\(a,b) -> (maybe 0 id (readMaybe (T.unpack a) :: Maybe Int), T.strip b)) . T.break (==' ') . fst . T.breakOn (T.pack " bag")) (T.splitOn (T.pack ", ") items))

processLineNoCount :: String -> (T.Text, [T.Text])
processLineNoCount s = let (bagName, contents) = (processLine s) in (bagName, map snd contents)

allContainers :: [(T.Text, [T.Text])] -> T.Text -> [T.Text]
allContainers db bag = let containers = [ fst dbItem | dbItem <- db, bag `elem` (snd dbItem)] in
  containers ++ (concatMap (allContainers db) containers)

bagContents :: [(T.Text, [(Int, T.Text)])] -> T.Text -> Int
bagContents db bag = let contents = maybe [] id (lookup bag db) in
  foldr (\(count, bagName) acc -> count * (bagContents db bagName) + acc) (sum $ map fst contents) contents

main = do
  contents <- readFile "bags.txt"
  let bagDb = map processLine $ lines contents
  print $ bagContents bagDb (T.pack "shiny gold")
  -- print $ length $ nub $ map T.unpack $ allContainers bagDb (T.pack "shiny gold")

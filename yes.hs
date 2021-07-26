module Yes where


import qualified Data.Text as T
import Data.Char
import Data.List

everyAnswer :: [T.Text] -> Int
everyAnswer ss = let options = (nub . T.unpack) (T.concat ss) in
  foldr (\c acc -> if all (\w -> c `elem` w) ss then acc + 1 else acc) 0 options


main = do
  contents <- readFile "yes.txt"
  print $ sum $ map (everyAnswer . T.lines) $ T.splitOn (T.pack "\n\n") $ T.pack contents

  --print $ sum $ map (length. nub . filter (not . isSpace) . T.unpack) $ T.splitOn (T.pack "\n\n") $ T.pack contents

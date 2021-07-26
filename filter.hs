filt :: (String, String) -> Bool
filt (a,b) = let l = length (filter (==last a) b) in l >= minCount && l <= maxCount 
  where 
    minCount = read (head $ splitByDelimiter '-' a) :: Int  
    maxCount = read ( head $ words $ splitByDelimiter '-' a !! 1) :: Int

filt' :: (String, String) -> (String, String)
filt' (a,b) = ((head $ splitByDelimiter '-' a) ,
    ( splitByDelimiter '-' a !! 1))

newFilt :: (String, String) -> Bool
newFilt (a,b) = (b !! firstIdx == last a || b !! sndIdx == last a) && not (
  (b !! firstIdx == last a && b !! sndIdx == last a)
  ) && (b !! firstIdx /= last a || b !! sndIdx /= last a)
  where 
    firstIdx = read (head $ splitByDelimiter '-' a) - 1 :: Int  
    sndIdx = read ( head $ words $ splitByDelimiter '-' a !! 1) - 1:: Int

splitByDelimiter :: Char -> String -> [String]
splitByDelimiter _ "" = []
splitByDelimiter delimiter list =
  map (takeWhile (/= delimiter) . tail)
    (filter (isPrefixOf [delimiter])
       (tails
           (delimiter : list)))
import Data.Maybe ( fromMaybe )
import Data.List ( foldl' )

directions :: [(Int, Char)]
directions = [(0, 'E'), (90, 'S'), (180, 'W'), (270, 'N')]

processInstruction :: [Char] -> (Int, Int, Int) -> (Int, Int, Int)
processInstruction [] s = s
processInstruction (instruction:charAmount) (startX, startY, startDir) =
    case instruction of
        'E' -> (startX + amount, startY, startDir)
        'W' -> (startX - amount, startY, startDir)
        'N' -> (startX, startY + amount, startDir)
        'S' -> (startX, startY - amount, startDir)
        'R' -> (startX, startY, (startDir + amount) `mod` 360)
        'L' -> (startX, startY, (startDir - amount) `mod` 360)
        -- fromMaybe below is dangerous because errors will not show
        'F' -> processInstruction (fromMaybe 'X' (lookup startDir directions):charAmount)
            (startX, startY, startDir)
        _ -> (startX, startY, startDir)
    where amount = read charAmount :: Int

turn :: (Int, Int) -> Int -> Char -> (Int, Int)
turn startCoords amount dir
    | amount == 0 = startCoords
    | otherwise = last $ take ((amount `div` 90) + 1) (iterate turnFun startCoords)
    where turnFun = if dir == 'L' then left90 else right90
          right90 (startX, startY) = (startY, negate startX)
          left90 (startX, startY) = (negate startY , startX)

processWaypointInstruction :: [Char] -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
processWaypointInstruction [] w s = (w, s)
processWaypointInstruction (instruction:charAmount) w@(wStartX, wStartY) s@(startX, startY) =
    case instruction of
        'E' -> ((wStartX + amount, wStartY), s)
        'W' -> ((wStartX - amount, wStartY), s)
        'N' -> ((wStartX, wStartY + amount), s)
        'S' -> ((wStartX, wStartY - amount), s)
        -- 90 Right turn: North becomes east, east becomes south, 
        -- south becomes west, west becomes north
        'R' -> (turn w amount 'R', s)
        -- 90 Left turn: North becomes west, west becomes south,
        -- south becomes east, east becomes north
        'L' -> (turn w amount 'L', s)
        'F' -> (w, (wStartX * amount + startX, wStartY * amount + startY))
        _ -> (w, s)
    where amount = read charAmount :: Int

main :: IO ()
main = do
  contents <- readFile "directions.txt"
  let instructions = lines contents
  -- Part 1
  let result' = foldl' (flip processInstruction) (0,0,0) instructions
  print result'
  print $ (\(x,y, _) -> abs x + abs y) result'
  putStrLn ""

  -- Part 2
  let result = foldl' (\(w, s) i -> processWaypointInstruction i w s) ((10, 1), (0, 0)) instructions
  print result
  print $ (\(_, (x,y)) -> abs x + abs y) result



--recursive, correct but unnecessary
goToWaypoint w s 0 = (w, s)
goToWaypoint w@(wStartX, wStartY) s@(startX, startY) times =
    goToWaypoint w (wStartX + startX, wStartY + startY) (times - 1)

loggedInstruction :: [Char] -> (Int, Int, Int) -> IO (Int, Int, Int)
loggedInstruction i s = do
    let r = processInstruction i s
    print r
    return r

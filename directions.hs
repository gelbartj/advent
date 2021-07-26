import Data.Maybe ( fromMaybe )
import Control.Monad
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

right90 (startX, startY) = (startY, negate startX)
left90 (startX, startY) = (negate startY , startX)

turn startCoords amount dir
    | amount == 0 = startCoords
    | otherwise = case dir of
        'L' -> turn (left90 startCoords) (amount - 90) dir
        'R' -> turn (right90 startCoords) (amount - 90) dir
        _ -> startCoords

--recursive, correct but unnecessary
goToWaypoint w s 0 = (w, s)
goToWaypoint w@(wStartX, wStartY) s@(startX, startY) times =
    goToWaypoint w (wStartX + startX, wStartY + startY) (times - 1)

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


loggedInstruction :: [Char] -> (Int, Int, Int) -> IO (Int, Int, Int)
loggedInstruction i s = do
    let r = processInstruction i s
    print r
    return r

main :: IO ()
main = do
  contents <- readFile "directions.txt"
  let instructions = lines contents
  -- Part 1
  -- let result = foldl' (flip processWaypointInstruction) (0,0,0) instructions
  let result = foldl' (\(w, s) i -> processWaypointInstruction i w s) ((10, 1), (0, 0)) instructions
  print result
  print $ (\(_, (x,y)) -> abs x + abs y) result
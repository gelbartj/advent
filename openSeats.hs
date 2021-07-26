import System.IO
import Data.Array
import Control.Monad ( forM_ )

makeArray :: [String] -> Array (Int, Int) Char
makeArray seatLines = listArray ((0,0), (length seatLines - 1, length (head seatLines) - 1))
    values where
        --indices = [(row, col) | row <- [0..length seatLines - 1], col <- [0..length (head seatLines) - 1]]
        values = concat seatLines


neighbors :: (Int, Int) -> Array (Int, Int) Char -> [Char]
neighbors (row, col) seatArray =
    let arrayRows = (fst.snd) $ bounds seatArray
        arrayCols = (snd.snd) $ bounds seatArray in
    [c |
        dr <- [- 1 .. 1],
        let newRow = dr + row,
        dc <- [- 1 .. 1],
        let newCol = dc + col,
        (newRow, newCol) /= (row, col),
        inRange (bounds seatArray) (newRow, newCol),
        let c = seatArray ! (newRow, newCol)]

visibleNeighbors :: (Int, Int) -> Array (Int, Int) Char -> [Char]
visibleNeighbors (row, col) seatArray =
    [firstInDirection (row, col) (r, c) |
   r <- [- 1 .. 1], c <- [- 1 .. 1], (r, c) /= (0, 0)] where
       firstInDirection (row', col') (r', c')
           | inRange (bounds seatArray) newCoord = if
               seatArray ! newCoord == '.' then
                   firstInDirection newCoord (r', c') else
                       seatArray ! newCoord
           | otherwise = '.'
           where newCoord = (row' + r', col' + c') 

toggleSeat :: Array (Int, Int) Char -> (Int, Int) -> Char
toggleSeat seatArray (row, col) = let currStatus = seatArray ! (row, col)
                                      seatNeighbors = visibleNeighbors (row, col) seatArray in
    case currStatus of
        'L' -> if '#' `notElem` seatNeighbors then '#' else 'L'
        '#' -> if length (filter (=='#') seatNeighbors) >= 5 then 'L' else '#'
        other -> other

cycleSeats :: Array (Int, Int) Char -> Int
cycleSeats seatArray = if toggledArray == seatArray then
    length (filter (=='#') (elems seatArray)) else
        cycleSeats toggledArray where
            toggledArray = listArray (bounds seatArray) (map (toggleSeat seatArray . fst) (assocs seatArray))

main = do
  contents <- readFile "openSeats.txt"
  let seatLines = lines contents
  let seatArray = makeArray seatLines
  print $ cycleSeats seatArray
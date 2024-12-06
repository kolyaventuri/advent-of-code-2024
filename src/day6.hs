import Util.String (splitOn)
import Data.List (nub)
import Debug.Trace (trace)

main :: IO()

findStartingPoint :: ([String], (Int, Int)) -> (Int, Int)
findStartingPoint ([], _) = error "No starting point"
findStartingPoint (lines, (x, y))
  | x < 0 || y < 0 = error "Starting point out of bounds"
  | y >= length lines = error "Starting point out of bounds"
  | x >= length (lines !! y) = findStartingPoint (lines, (0, y + 1))
  | (lines !! y) !! x == '^' = (x, y)
  | otherwise = findStartingPoint (lines, (x + 1, y))

printMap :: ([String], [(Int, Int)], (Int, Int), String) -> String
printMap (lines, steps, (x, y), curr)
  | x >= length lines = printMap (lines, steps, (0, y + 1), curr ++ "\n")
  | y >= length (lines !! x) = curr 
  | any (\(x',y') -> x == x' && y == y') steps = printMap (lines, steps, (x + 1, y), curr ++ [hitChar])
  | otherwise = printMap (lines, steps, (x + 1, y), curr ++ [currentChar])
  where
    currentChar = (lines !! y) !! x
    hitChar = if (x,y) == head steps then 'O' else 'X'


-- Directions:
-- (0, -1) - up
-- (1, 0) - right
-- (0, 1) - down
-- (-1, 0) - left
walkMap :: ([String], (Int, Int), (Int, Int), [(Int, Int)]) -> [(Int, Int)] 
walkMap ([], _, _, _) = error "No map"
walkMap (lines, (x, y), direction, steps)
  -- | trace ("Moving " ++ show direction ++ " -- Step " ++ show (length steps) ++ "\n" ++ printMap(lines, steps, (0,0), "")) False = undefined
  | y < 0 || x < 0 = steps -- Base case, we made it out
  | nextY < 0 || nextX < 0 = steps -- Base case, we made it out
  | y >= length lines - 1 = steps -- Base case, we made it out
  | x >= length (lines !! y) - 1 = steps -- Base case, we made it out
  | hitBarrel = walkMap (lines, (nextX, nextY), nextDirection, (nextX, nextY) : steps)
  | otherwise = walkMap (lines, (nextX, nextY), nextDirection, (nextX, nextY) :steps)
  where
    checkX = if (x + fst direction) >= 0 && (x + fst direction) < length (lines !! y) then x + fst direction else x
    checkY = if (y + snd direction) >= 0 && (y + snd direction) < length lines then y + snd direction else y
    bY = lines !! checkY
    bX = (lines !! checkY) !! checkX
    hitBarrel = bX == '#'
    nextDirection
      | not hitBarrel = direction
      | fst direction == 0 && snd direction == -1 = (1, 0) -- Up -> Right
      | fst direction == 1 && snd direction == 0 = (0, 1) -- Right -> Down
      | fst direction == 0 && snd direction == 1 = (-1, 0) -- Down -> Left
      | fst direction == -1 && snd direction == 0 = (0, -1) -- Left -> Up
      | otherwise = error "Invalid direction"
    nextX = x + fst nextDirection
    nextY = y + snd nextDirection

main = do
  contents <- readFile "input/day6.txt"
  let chunks = splitOn "\n" contents
  let lines = filter (not . null) chunks

  let startingPoint = findStartingPoint (lines, (0, 0))
  print startingPoint

  let p1Steps = walkMap (lines, startingPoint, (0, -1), [])
  let p1Uniq = nub p1Steps
  let part1 = length p1Uniq 
  
  putStrLn ("Part 1: " ++ show part1)

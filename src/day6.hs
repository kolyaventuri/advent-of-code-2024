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


getNextValidDirection :: ([String], (Int, Int), (Int, Int)) -> (Int, Int)
getNextValidDirection (lines, (x, y), (dx, dy))
  | not hitBarrel = (dx, dy)
  | dx == 0 && dy == -1 = getNextValidDirection (lines, (x, y), (1, 0)) -- Up -> Right
  | dx == 1 && dy == 0 = getNextValidDirection (lines, (x, y), (0, 1)) -- Right -> Down
  | dx == 0 && dy == 1 = getNextValidDirection (lines, (x, y), (-1, 0)) -- Down -> Left
  | dx == -1 && dy == 0 = getNextValidDirection (lines, (x, y), (0, -1)) -- Left -> Up 
  | otherwise = error "Invalid direction"
  where
    checkX = x + dx
    checkY = y + dy
    _maxX = length lines - 1
    _maxY = length (head lines) - 1
    hitBarrel = not ((checkX < 0 || checkX > _maxX) || (checkY < 0 || checkY > _maxY)) && ((lines !! checkY) !! checkX == '#')

getNextValidDirectionBlocked :: ([String], (Int, Int), (Int, Int), (Int, Int)) -> (Int, Int)
getNextValidDirectionBlocked (lines, (x, y), blocked, (dx, dy))
  | not hitBarrel = (dx, dy)
  | dx == 0 && dy == -1 = getNextValidDirection (lines, (x, y), (1, 0)) -- Up -> Right
  | dx == 1 && dy == 0 = getNextValidDirection (lines, (x, y), (0, 1)) -- Right -> Down
  | dx == 0 && dy == 1 = getNextValidDirection (lines, (x, y), (-1, 0)) -- Down -> Left
  | dx == -1 && dy == 0 = getNextValidDirection (lines, (x, y), (0, -1)) -- Left -> Up 
  | otherwise = error "Invalid direction"
  where
    checkX = x + dx
    checkY = y + dy
    _maxX = length lines - 1
    _maxY = length (head lines) - 1
    hitBarrel = not ((checkX < 0 || checkX > _maxX) || (checkY < 0 || checkY > _maxY)) && ((lines !! checkY) !! checkX == '#') || (checkX == fst blocked && checkY == snd blocked)

-- Directions:
-- (0, -1) - up
-- (1, 0) - right
-- (0, 1) - down
-- (-1, 0) - left
walkMap :: ([String], (Int, Int), (Int, Int), [(Int, Int)]) -> [(Int, Int)]
walkMap ([], _, _, _) = error "No map"
walkMap (lines, (x, y), direction, steps)
  -- | trace ("Moving " ++ show direction ++ " -- Step " ++ show (length steps) ++ "\n" ++ printMap(lines, steps, (0,0), "")) False = undefined
  -- | trace ("At " ++ show (x, y) ++ ", Moving " ++ show direction ++ " -- Step " ++ show (length steps)) False = undefined
  -- | trace ("  next: " ++ show (nextX, nextY)) False = undefined
  | (lines !! y) !! x == '#' = error "I'm standing on a barrel. Something is wrong."
  | y < 0 || x < 0 = steps -- Base case, we made it out
  | nextY < 0 || nextX < 0 = steps -- Base case, we made it out
  | nextY > length lines - 1 = steps -- Base case, we made it out
  | nextX > length (lines !! y) - 1 = steps -- Base case, we made it out
  | otherwise = walkMap (lines, (nextX, nextY), nextDirection, (nextX, nextY) : steps)
  where
    checkX = x + fst direction
    checkY = y + snd direction
    _maxX = length lines - 1
    _maxY = length (head lines) - 1
    nextDirection = getNextValidDirection (lines, (x, y), direction)
    nextX = x + fst nextDirection
    nextY = y + snd nextDirection

createsLoop :: ([String], (Int, Int), (Int, Int), (Int, Int), [(Int, Int, (Int, Int))]) -> Bool
createsLoop ([], _, _, _, _) = error "No map"
createsLoop (lines, (x, y), blocked, direction, steps)
  -- | trace ("Moving " ++ show direction ++ " -- Step " ++ show (length steps) ++ "\n" ++ printMap (lines, map (\(x,y,_) -> (x,y)) steps, (0,0), "")) False = undefined
  -- | trace ("At " ++ show (x, y) ++ ", Moving " ++ show direction ++ " -- Step " ++ show (length steps)) False = undefined
  -- | trace (" x=" ++ show x ++ " y=" ++ show y ++ " dX=" ++ show dX ++ " dY=" ++ show dY ++ "/" ++ show steps ) False = undefined
  | (lines !! y) !! x == '#' = error "I'm standing on a barrel. Something is wrong."
  | y < 0 || x < 0 = False --Base case, we made it out
  | nextY < 0 || nextX < 0 = False -- Base case, we made it out
  | nextY > length lines - 1 = False -- Base case, we made it out
  | nextX > length (lines !! y) - 1 = False -- Base case, we made it out
  | testSpot = True
  | otherwise = createsLoop (lines, (nextX, nextY), blocked, nextDirection, (x, y, direction) : steps)
  where
    checkX = x + fst direction
    checkY = y + snd direction
    _maxX = length lines - 1
    _maxY = length (head lines) - 1
    nextDirection = getNextValidDirectionBlocked (lines, (x, y), blocked, direction)
    dX = fst nextDirection
    dY = snd nextDirection
    nextX = x + dX
    nextY = y + dY
    testSpot
      | valid && trace (printMap (lines, map (\(x,y,_) -> (x,y)) steps, (0,0), "")) False = undefined
      | otherwise = valid
      where
        valid = any (\(x', y', (dX', dY')) -> x == x' && y == y' && fst direction == dX' && snd direction == dY') steps


checkPossibleLoops :: ([String], (Int, Int), [(Int, Int)]) -> Int
checkPossibleLoops (lines, (x, y), blocked) = sum results
  where
    doCheckLoop :: (Int, (Int, Int)) -> Int
    doCheckLoop (id, point)
      -- | trace ("Checking loop " ++ show (id + 1) ++ " of " ++ show (length blocked) ++ " at " ++ show point) False = undefined
      | createsLoop (lines, (x, y), point, (0, -1), []) = 1
      | otherwise = 0
    withIds = zip [0..] blocked
    results = map doCheckLoop withIds

main = do
  contents <- readFile "input/day6.test.txt"
  let chunks = splitOn "\n" contents
  let lines = filter (not . null) chunks

  let startingPoint = findStartingPoint (lines, (0, 0))
  print startingPoint

  let p1Steps = walkMap (lines, startingPoint, (0, -1), [startingPoint])
  let p1Uniq = nub p1Steps
  let part1 = length p1Uniq

  putStrLn ("Part 1: " ++ show part1)

  putStrLn ""

  let p2BlockedRoutes = checkPossibleLoops (lines, startingPoint, p1Uniq)

  putStrLn ("Part 2: " ++ show p2BlockedRoutes)

import Util.String (splitOn, slice)
import Data.Foldable (forM_)
import Data.List (transpose)

main :: IO()

printGrid :: [String] -> IO()
printGrid grid = do
  let rows = length grid
  let cols = max (length (head grid)) (length (last grid))
  forM_ [0..(rows - 1)] $ \row -> do
    forM_ [0..(cols - 1)] $ \col -> do
      let char = if col > length (grid !! row) - 1 then '.' else grid !! row !! col
      putStr [char]
    putStrLn ""

processLine :: (String, Int) -> Int
processLine ([], _) = 0
processLine (line, index)
  | index >= length line - 3 = 0
  | found = 1 + processLine (line, index + 1)
  | otherwise = processLine (line, index + 1)
  where
    part = slice index (index + 4) line
    found = part == "XMAS" || part == "SAMX"

findHorizontals :: [String] -> Int
findHorizontals [] = 0
findHorizontals grid = sum $ map (\line -> processLine (line, 0)) grid

findVerticals :: [String] -> Int
findVerticals [] = 0
findVerticals grid = sum $ map (\line -> processLine (line, 0)) (transpose grid)

leftPad :: String -> Int -> String
leftPad str count = replicate count '.' ++ str

rightPad:: String -> Int -> String
rightPad str count = str ++ replicate count '.'

diagonalsRShift :: [String] -> [String]
diagonalsRShift grid = reverse (zipWith leftPad (reverse grid) [0..])

diagonalsLShift :: [String] -> [String]
diagonalsLShift grid = zipWith leftPad grid [0..]

findDiags :: [String] -> Int
findDiags [] = 0
findDiags grid = left + right
  where
    right = findVerticals $ diagonalsRShift grid
    left = findVerticals $ diagonalsLShift grid

searchXMASP1 :: [String] -> Int
searchMASP1 :: Num a1 => [a2] -> a1
searchMASP1 [] = 0
searchXMASP1 grid = findHorizontals grid + findVerticals grid + findDiags grid

isCross :: ([Int], [String]) -> Int
isCross (_, []) = 0
isCross (coords, grid)
  | (grid !! y) !! x /= 'A' = 0 -- Middle
  | all (\c -> c == "MAS" || c == "SAM") options = 1
  | otherwise = 0
  where
    y = head coords
    x = coords !! 1
    topLeft = grid !! (y - 1) !! (x - 1)
    topRight = grid !! (y - 1) !! (x + 1)
    bottomLeft = grid !! (y + 1) !! (x - 1)
    bottomRight = grid !! (y + 1) !! (x + 1)
    options = [[topLeft] ++ "A" ++ [bottomRight], [topRight] ++ "A" ++ [bottomLeft]]

identifyPossibleCenters :: [String] -> [[Int]]
identifyPossibleCenters [] = []
identifyPossibleCenters input = filter (\c -> head c /= 0 && (c !! 1) /= maxY && (c !! 1) /= 0 && head c /= maxX) allAs
  where
    allAs = [ [x, y] | (x, row) <- zip [0..] input, (y, char) <- zip [0..] row, char == 'A']
    maxY = length input - 1
    maxX = length (head input) - 1

findCrosses :: [String] -> Int
findCrosses [] = 0
findCrosses grid = sum $ map (\coord -> isCross (coord, grid)) coords
  where
    coords = identifyPossibleCenters grid

searchXMAS2 :: [String] -> Int
searchXMAS2 [] = 0
searchXMAS2 grid = findCrosses grid

main = do
  contents <- readFile "input/day4.txt"
  let lines = splitOn "\n" contents
  let parts = filter (not . null) lines

  let part1 = searchXMASP1 parts

  putStrLn ("Part 1: " ++ show part1)

  let part2 = searchXMAS2 parts

  putStrLn ("Part 2: " ++ show part2)

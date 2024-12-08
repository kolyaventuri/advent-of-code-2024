import Util.String (splitOn)
import Data.List (sort, nub)
import Data.Foldable (forM_)
import qualified Data.Map as Map
import Debug.Trace (trace)

main :: IO ()

parseMap :: ([String], Map.Map Char [(Int, Int)], (Int, Int)) -> Map.Map Char [(Int, Int)]
parseMap (input, localMap, (y, x))
  | y == length input = localMap
  | x == length (input !! y) = parseMap (input, localMap, (y + 1, 0))
  | (input !! y) !! x == '.' = parseMap (input, localMap, (y, x + 1))
  | otherwise = parseMap (input, Map.insertWith (++) ((input !! y) !! x) [(y, x)] localMap, (y, x + 1))

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

getAntinodes :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
getAntinodes ((y1, x1), (y2, x2))
  -- | trace ("(" ++ show y1 ++ ", "  ++ show x1 ++ ") -> (" ++ show y2 ++ ", " ++ show x2 ++ ") == " ++ show (y2 - dY, x2 - dX) ++ " -> " ++ show (y1 + dY, x1 + dX)) False = undefined
  = ((y1 + dY, x1 + dX), (y2 - dY, x2 - dX))
  where
    dX = x1 - x2
    dY = y1 - y2

plotAntinodes :: ((Int, Int), [(Int, Int)], [(Int, Int)]) -> [(Int, Int)]
plotAntinodes (curr, nodes, result)
  | null nodes = result
  | otherwise = plotAntinodes (head t, t, result ++ next)
  where
    t = tail nodes
    next = if null t then [] else concatMap (\x -> pairToList (getAntinodes (curr, x))) t

printMap :: ([String], [(Int, Int)]) -> IO()
printMap (grid, nodes) = do
  let rows = length grid
  let cols = max (length (head grid)) (length (last grid))
  forM_ [0..(rows - 1)] $ \row -> do
    forM_ [0..(cols - 1)] $ \col -> do
      let hasAntinode = any (\(y, x) -> y == row && x == col) nodes
      let char = if col > length (grid !! row) - 1 then '.' else grid !! row !! col
      putStr (if hasAntinode then "#" else [char])
    putStrLn ""

main = do
  contents <- readFile "input/day8.txt"
  let lines = splitOn "\n" contents
  let filteredLines = filter (not . null) lines

  let h = length filteredLines
  let w = length (head filteredLines)

  let positions = parseMap (filteredLines, Map.empty, (0, 0))
  let pos = Map.elems positions

  let antinodes = map (\x -> plotAntinodes (head x, x, [])) pos

  let flattened = concat antinodes
  --print flattened
  --print (length flattened)

  let filtered = filter (\(y, x) -> x >= 0 && x < w && y >=0 && y < h) flattened
  let unique = nub filtered

  --print unique

  --printMap (filteredLines, flattened)

  let part1 = length unique
  putStrLn $ "Part 1: " ++ show part1





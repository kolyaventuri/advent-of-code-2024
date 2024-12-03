import Util.Split (splitOn)
import Data.List (genericIndex)

main :: IO ()

pop :: [a] -> [a]
pop [] = []
pop (_:xs) = xs

stepThroughP1 :: ([Int], Int) -> Int
stepThroughP1 ([], _) = 0
stepThroughP1 ([x], _) = 1
stepThroughP1 (list, direction)
  | diff < 1 || diff > 3 = 0
  | direction /= (if second > first then 1 else -1) = 0
  | otherwise = stepThroughP1 (pop list, direction)
  where
    first = head list
    second = list !! 1
    diff = abs (first - second)

removeEveryIndex :: [a] -> [[a]]
removeEveryIndex xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

stepThroughP2 :: [Int] -> Int
stepThroughP2 [] = 0
stepThroughP2 [x] = 1
stepThroughP2 list
  | anyValid = 1 
  | otherwise = 0
  where
    totalArrays = removeEveryIndex list ++ [list]
    results = calculateValidityP1 totalArrays
    anyValid = any (> 0) results

calculateValidityP1 :: [[Int]] -> [Int]
calculateValidityP1 = map process
  where
    process arr@(x1:x2:_) = stepThroughP1 (arr, if x2 > x1 then 1 else -1)
    process _ = 0

calculateValidityP2 :: [[Int]] -> [Int]
calculateValidityP2 = map process
  where
    process arr@(x1:x2:_) = stepThroughP2 arr
    process _ = 0

main = do
  contents <- readFile "input/day2.txt"
  let lines = splitOn "\n" contents
  let parts = filter (not . null) (map words lines)
  let numbers = map (map read :: [String] -> [Int]) parts

  let validity = calculateValidityP1 numbers
  let numValid = sum validity

  putStrLn $ "Part 1: " ++ show numValid

  let validityP2 = calculateValidityP2 numbers
  let numValidP2 = sum validityP2

  putStrLn $ "Part 2: " ++ show numValidP2




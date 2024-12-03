import Util.Split (splitOn)
import Data.List (genericIndex)

main :: IO ()

pop :: [a] -> [a]
pop [] = []
pop (_:xs) = xs

stepThrough :: ([Int], Int) -> Int 
stepThrough ([], _) = 0 
stepThrough ([x], _) = 1 
stepThrough (list, direction)
  | diff < 1 || diff > 3 = 0 
  | direction /= (if second > first then 1 else -1) = 0 
  | otherwise = stepThrough (pop list, direction)
  where
    first = head list
    second = list !! 1
    diff = abs (first - second)

calculateValidity :: [[Int]] -> [Int]
calculateValidity = map process
  where
    process arr@(x1:x2:_) = stepThrough (arr, if x2 > x1 then 1 else -1)
    process _ = 0 

main = do
  contents <- readFile "input/day2.txt"
  let lines = splitOn "\n" contents
  let parts = filter (not . null) (map words lines)
  let numbers = map (map read :: [String] -> [Int]) parts

  let validity = calculateValidity numbers
  let numValid = sum validity 

  putStrLn $ "Part 1: " ++ show numValid




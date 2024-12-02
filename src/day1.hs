import Util.Split (splitOn)
import Data.List (sort)

main :: IO ()

getDistance :: ([Int], [Int]) -> [Int]
getDistance (left, right) = zipWith subtract left right

main = do
  contents <- readFile "input/day1.test.txt"
  let lines = splitOn "\n" contents
  let parts = filter (not . null) (map words lines)
  let numbers = map (map read :: [String] -> [Int]) parts

  let sortLeft = sort (map head numbers)
  let sortRight = sort (map last numbers)

  let distance = getDistance (sortLeft, sortRight)
  let part1 = sum distance

  putStrLn $ "Part 1: " ++ show part1



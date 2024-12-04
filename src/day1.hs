import Util.String (splitOn)
import Data.List (sort)
import qualified Data.Map as Map

main :: IO ()

getDistance :: ([Int], [Int]) -> [Int]
getDistance (left, right) = zipWith subtract left right

countOccurrences :: (Ord a) => [a] -> Map.Map a Int
countOccurrences = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty

getSimilarity :: [Int] -> Map.Map Int Int -> [Int]
getSimilarity list counts = map (\x -> Map.findWithDefault 0 x counts * x) list

main = do
  contents <- readFile "input/day1.txt"
  let lines = splitOn "\n" contents
  let parts = filter (not . null) (map words lines)
  let numbers = map (map read :: [String] -> [Int]) parts

  let sortLeft = sort (map head numbers)
  let sortRight = sort (map last numbers)

  let distance = getDistance (sortLeft, sortRight)
  let part1 = sum (map abs distance)

  putStrLn $ "Part 1: " ++ show part1

  let counts = countOccurrences sortRight
  let similarities = getSimilarity sortLeft counts

  let part2 = sum similarities

  putStrLn $ "Part 2: " ++ show part2





import Util.String (splitOn, slice)
import Debug.Trace (trace)

main :: IO ()

toInt :: String -> Int
toInt str = read str :: Int

checkEquationValidity :: (Int, [Int]) -> Bool
checkEquationValidity (result, numbers)
  -- | trace (show result ++ " " ++ show numbers) False = undefined
  | null numbers = abs result < 2 
  | mod result (head numbers) == 0 = checkEquationValidity (div result (head numbers), tail numbers) || checkEquationValidity (result - head numbers, tail numbers)
  | otherwise = checkEquationValidity (result - head numbers, tail numbers)

main = do
  contents <- readFile "input/day7.txt"
  let lines = splitOn "\n" contents
  let filtered = filter (not . null) (map words lines)

  let cleaned = map (\x -> (toInt (slice 0 (length (head x) - 1) (head x)), map toInt (reverse (tail x)))) filtered

  let valid = filter checkEquationValidity cleaned

  let part1 = sum (map fst valid)

  putStrLn $ "Part 1: " ++ show part1


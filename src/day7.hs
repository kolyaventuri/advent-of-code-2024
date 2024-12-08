import Util.String (splitOn, slice)
import Debug.Trace (trace)

main :: IO ()

toInt :: String -> Int
toInt str = read str :: Int

checkEquationValidityP1 :: (Int, [Int]) -> Bool
checkEquationValidityP1 (result, numbers)
  -- | trace (show result ++ " " ++ show numbers) False = undefined
  | null numbers = abs result < 2
  | mod result (head numbers) == 0 = checkEquationValidityP1 (div result (head numbers), tail numbers) || checkEquationValidityP1 (result - head numbers, tail numbers)
  | otherwise = checkEquationValidityP1 (result - head numbers, tail numbers)

checkEquationValidityP2 :: (Int, [Int]) -> Bool
checkEquationValidityP2 (result, numbers)
  | trace (show result ++ " " ++ show numbers ++ " " ++ show concat) False = undefined
  | null numbers = abs result < 2
  | mod result (head numbers) == 0 = checkEquationValidityP2 (div result (head numbers), tail numbers) || checkEquationValidityP2 (result - head numbers, tail numbers)
  | Just c <- concat, mod result c == 0 = checkEquationValidityP2 (div result c, c : drop 2 numbers) || checkEquationValidityP2 (result - c, c : drop 2 numbers)
  | otherwise = checkEquationValidityP2 (result - head numbers, tail numbers)
  where
    concat = if length numbers > 1 then Just (toInt (show (numbers !! 1) ++ show (head numbers))) else Nothing

main = do
  contents <- readFile "input/day7.test.txt"
  let lines = splitOn "\n" contents
  let filtered = filter (not . null) (map words lines)

  let cleaned = map (\x -> (toInt (slice 0 (length (head x) - 1) (head x)), map toInt (reverse (tail x)))) filtered

  let validP1 = filter checkEquationValidityP1 cleaned
  let part1 = sum (map fst validP1)

  putStrLn $ "Part 1: " ++ show part1

  -- let validP2 = filter checkEquationValidityP2 cleaned
  -- print validP2
  -- let part2 = sum (map fst validP2)

  -- putStrLn $ "Part 2: " ++ show part2

  print (checkEquationValidityP2 (7290, [15, 6, 8 ,6]))


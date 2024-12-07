import Util.String (splitOn, slice)
import qualified Data.Map as Map

main :: IO ()

toInt :: String -> Int
toInt str = read str :: Int

main = do
  contents <- readFile "input/day7.test.txt"
  let lines = splitOn "\n" contents
  let filtered = filter (not . null) (map words lines)
  
  let cleaned = map (\x -> (toInt (slice 0 (length (head x) - 1) (head x)), map toInt (tail x))) filtered
  let map = Map.fromList cleaned

  let x = Map.lookup 3267 map

  print x 



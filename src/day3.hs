import Util.Split (splitOn)
import Data.Char (isDigit)

main :: IO ()

extractNumbers :: String -> [Int]
extractNumbers [] = []
extractNumbers input = map (read :: String -> Int) parts
  where
    parts = splitOn "," input

readArgs :: (String, Int) -> [Int]
readArgs ([], _) = []
readArgs (input, index)
  | index == length input - 1 = []
  | input !! index == ')' = extractNumbers (take index input)
  | not (isDigit (input !! index)) && input !! index /= ',' = []
  | otherwise = readArgs (input, index + 1)

readInstructions :: (String, String) -> [[Int]]
readInstructions (input, name) = processChunks input
  where
    instructionLength = length name + 1 -- Add one to account for first parenthesis
    processChunks [] = []
    processChunks str
      | length str < instructionLength = [[]]
      | take (length name) str == name = readArgs (drop instructionLength str, 0) : processChunks (drop instructionLength str)
      | otherwise = processChunks (drop 1 str)

main = do
  contents <- readFile "input/day3.txt"

  let rawInstructions = readInstructions (contents, "mul")
  let filteredInstructions = filter (not . null) rawInstructions

  let evaluated = map product filteredInstructions
  let part1 = sum evaluated

  putStrLn $ "Part 1: " ++ show part1


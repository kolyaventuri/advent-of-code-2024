import Util.String (splitOn)
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

processChunksP2 :: (String, String, Bool) -> [[Int]]
processChunksP2 (_, [], _) = []
processChunksP2 (name, str, doRead)
  | length str < instructionLength = [[]]
  | take 7 str == "don't()" = processChunksP2 (name, drop 7 str, False)
  | take 4 str == "do()" = processChunksP2 (name, drop 4 str, True)
  | take (length name) str == name && doRead = readArgs (drop instructionLength str, 0) : processChunksP2 (name, drop instructionLength str, doRead)
  | otherwise = processChunksP2 (name, drop 1 str, doRead)
  where
    instructionLength = length name + 1 -- Add one to account for first parenthesis

readInstructionsP2 :: (String, String) -> [[Int]]
readInstructionsP2 (input, name) = processChunksP2 (name, input, True)

main = do
  contents <- readFile "input/day3.txt"

  let rawInstructions = readInstructions (contents, "mul")
  let filteredInstructions = filter (not . null) rawInstructions

  let evaluated = map product filteredInstructions
  let part1 = sum evaluated

  putStrLn $ "Part 1: " ++ show part1

  let p2Raw = readInstructionsP2 (contents, "mul")
  let filteredInstructionsP2 = filter (not . null) p2Raw

  let evaluatedP2 = map product filteredInstructionsP2
  let part2 = sum evaluatedP2


  putStrLn $ "Part 2: " ++ show part2


import Util.String (splitOn, slice)
import Data.List (sortBy)

main :: IO ()

stackValid:: ([[Int]], [Int]) -> Bool
stackValid (ordering, stack)
  | null stack = True
  | all (\x -> tail x `notElem` stack') rules = stackValid (ordering, tail stack)
  | otherwise = False
  where
    rules = filter (\x -> head x == head stack) ordering
    stack' = map (:[]) stack
  
separateValids :: ([[Int]], [[Int]]) -> [[[Int]]]
separateValids ([], []) = []
separateValids (ordering, pages)
  | null valids = []
  | otherwise = [valids, invalids] 
  where 
    valids = filter (\page -> stackValid (ordering, reverse page)) pages 
    invalids = filter (\page -> not (stackValid (ordering, reverse page))) pages 

part1Mids :: [[Int]] -> [Int]
part1Mids [] = []
part1Mids arr = map (\x -> x !! (length x `div` 2)) arr

fixInvalids :: ([[Int]], [Int]) -> [Int]
fixInvalids (ordering, pages) = sortBy sortPairs pages
  where
    sortPairs a b = if stackValid (ordering, [b, a]) then EQ else GT

main = do
  contents <- readFile "input/day5.txt"
  let chunks = splitOn "\n\n" contents

  let orderingStrings = splitOn "\n" (head chunks)
  let pageStrings = filter (not . null) (splitOn "\n" (chunks !! 1))

  let oPairs = map (splitOn "|") orderingStrings
  let ordering = map (map read :: [String] -> [Int]) oPairs

  let pGroups = map (splitOn ",") pageStrings
  let pages = map (map read :: [String] -> [Int]) pGroups

  -- print ordering
  -- print pages

  let separated = separateValids (ordering, pages)
  let valid = head separated
  let invalid = last separated

  let p1Mids = part1Mids valid 
  let part1 = sum p1Mids
  
  putStrLn ("Part 1: " ++ show part1)

  putStrLn ""

  let fixed = map (fixInvalids . (ordering,)) invalid
  let p2Mids = part1Mids fixed
  let part2 = sum p2Mids

  putStrLn ("Part 2: " ++ show part2)

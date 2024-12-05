import Util.String (splitOn, slice)

main :: IO ()

stackValid:: ([[Int]], [Int]) -> Bool
stackValid (ordering, stack)
  | null stack = True
  | all (\x -> tail x `notElem` stack') rules = stackValid (ordering, tail stack)
  | otherwise = False
  where
    rules = filter (\x -> head x == head stack) ordering
    stack' = map (:[]) stack

part1ValidOrders :: ([[Int]], [[Int]]) -> [[Int]]
part1ValidOrders ([], []) = []
part1ValidOrders ([], _) = []
part1ValidOrders (ordering, pages) = filter (\page -> stackValid (ordering, reverse page)) pages

part1Mids :: [[Int]] -> [Int]
part1Mids [] = []
part1Mids arr = map (\x -> x !! (length x `div` 2)) arr

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

  let p1Valids = part1ValidOrders (ordering, pages)
  let p1Mids = part1Mids p1Valids 
  let part1 = sum p1Mids
  
  putStrLn ("Part 1: " ++ show part1)

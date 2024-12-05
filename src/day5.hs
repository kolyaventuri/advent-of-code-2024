import Util.String (splitOn)

main :: IO ()

filterOrdering :: ([[Int]], [Int], [Int]) -> [Int]
filterOrdering ([], [], _) = []
filterOrdering ([], _, _) = []
filterOrdering (ordering, pages, stack)
  | length stack == length pages = stack
  | otherwise = filterOrdering (ordering, pages, stack ++ [next])
  where
    next = pages !! length stack
    current = if null stack then head pages else pages !! (length stack - 1)
    rules = all (\x -> head x == next) ordering

part1Ordering :: ([[Int]], [[Int]], [Int]) -> [Int]
part1Ordering ([], [], _) = []
part1Ordering ([], _, _) = []
part1Ordering (ordering, pages, stack)
  | length stack == length pages = stack
  | otherwise = []

main = do
  contents <- readFile "input/day5.test.txt"
  let chunks = splitOn "\n\n" contents

  let orderingStrings = splitOn "\n" (head chunks)
  let pageStrings = filter (not . null) (splitOn "\n" (chunks !! 1))

  let oPairs = map (splitOn "|") orderingStrings
  let ordering = map (map read :: [String] -> [Int]) oPairs 

  let pGroups = map (splitOn ",") pageStrings
  let pages = map (map read :: [String] -> [Int]) pGroups

  print ordering
  print pages 

  let part1 = part1Ordering (ordering, pages, [])

  putStrLn ""
  print (filterOrdering (ordering, head pages, []))

  putStrLn $ "Part 1: " ++ show part1

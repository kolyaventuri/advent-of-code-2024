import Util.String (splitOn)

main :: IO ()

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

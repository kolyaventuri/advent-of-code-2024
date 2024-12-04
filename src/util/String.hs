module Util.String (slice, splitOn) where

slice :: Int -> Int -> String -> String
slice a b = take (b - a) . drop a

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delimiter str = go str
  where
    go [] = [[]]
    go s
      | delimiter `isPrefixOf` s = [] : go (drop (length delimiter) s)
      | otherwise =
          let (first:rest) = go (tail s)
           in (head s : first) : rest
    isPrefixOf prefix xs = take (length prefix) xs == prefix

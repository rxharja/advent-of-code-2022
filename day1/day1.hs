-- Day 1: Calorie Counting
-- 1\n2\n3\n\n4\n5\n
-- ["1\n2\n3\n", "4\n5\n"]
-- [[1,2,3], [4,5]]
-- [6, 9]
-- 9

module Main where

wordsWhen :: String -> [String]
wordsWhen xs = go xs "" []
  where
    go "" _ acc = acc
    go ('\n' : '\n' : xs) sub acc = go xs "" (acc ++ [sub])
    go (x : xs) sub acc = go xs (sub ++ [x]) acc

main :: IO ()
main = do
  calories <- readFile "day1-input.txt"
  let x = maximum . map (sum . map read . words) . wordsWhen $ calories
  print x

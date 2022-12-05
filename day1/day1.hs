-- Day 1: Calorie Counting

module Main where

wordsWhen :: String -> [String]
wordsWhen xs = go xs "" []
  where
    go "" _ acc = acc
    go ('\n' : '\n' : xs) sub acc = go xs "" (acc ++ [sub])
    go (x : xs) sub acc = go xs (sub ++ [x]) acc

maxCalories :: String -> Int
maxCalories = maximum . map (sum . map read . words) . wordsWhen

main :: IO ()
main = do
  calories <- readFile "day1-input.txt"
  print $ maxCalories calories

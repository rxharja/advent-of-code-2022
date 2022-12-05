-- Day 1: Calorie Counting

module Main where

splitOnTwoNewLines :: String -> [String]
splitOnTwoNewLines xs = go xs "" []
  where
    go "" _ acc = acc
    go ('\n' : '\n' : xs) sub acc = go xs "" (acc ++ [sub])
    go (x : xs) sub acc = go xs (sub ++ [x]) acc

sumCalories :: String -> Int
sumCalories = sum . map read . words

maxCalories :: String -> Int
maxCalories = maximum . map sumCalories . splitOnTwoNewLines

main :: IO ()
main = do
  calories <- readFile "day1-input.txt"
  print $ maxCalories calories

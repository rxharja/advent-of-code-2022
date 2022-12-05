-- Day 1: https://adventofcode.com/2022/day/1
module Main where

import Data.List (sort)

splitOnTwoNewLines :: String -> [String]
splitOnTwoNewLines xs = go xs "" []
  where
    go "" _ acc = acc
    go ('\n' : '\n' : xs) sub acc = go xs "" (acc ++ [sub])
    go (x : xs) sub acc = go xs (sub ++ [x]) acc

elfCalories :: String -> [Int]
elfCalories = map (sum . map read . words) . splitOnTwoNewLines

main :: IO ()
main = do
  input <- readFile "day1-input.txt"
  let calories = elfCalories input
  let top3 = (take 3 . reverse . sort) calories

  putStr "Elf with Max Calories: "
  print $ maximum calories

  putStr "Top 3 Calories: "
  print top3

  putStr "sum of Top 3 Calories: "
  print $ sum top3

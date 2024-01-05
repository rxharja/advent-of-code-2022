-- Day 1: https://adventofcode.com/2022/day/1
module Main where

import Data.List (groupBy, sort)

parseTotalCalories :: String -> [Int]
parseTotalCalories = map (sum . map read . filter (/= "")) . groupBy (\_ b -> b /= "") . lines

top :: Ord a => Int -> [a] -> [a]
top n = take n . reverse . sort

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStr "Max Calories: "; print . head . top 1 . parseTotalCalories $ input
  putStr "Top 3 Calories Combined: "; print . sum . top 3 . parseTotalCalories $ input

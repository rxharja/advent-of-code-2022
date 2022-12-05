-- Day 2: https://adventofcode.com/2022/day/2
module Main where

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

splitIntoPairs :: String -> (Char, Char)
splitIntoPairs s = (head fst, head lst)
  where
    l = wordsWhen (== ' ') s
    fst = head l
    lst = last l

main :: IO ()
main = do
  input <- readFile "day2-input.txt"
  let pairs = (map splitIntoPairs . words) input
  print pairs
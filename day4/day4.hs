module Main where

type Pairs = ((Int, Int), (Int, Int))

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

parse :: String -> Pairs
parse s = ((read $ head fstPair, read $ last fstPair), (read $ head lstPair, read $ last lstPair))
  where
    splitWords = wordsWhen (== ',')
    splitDash = wordsWhen (== '-')
    pairs = map splitDash . splitWords
    fstPair = head . pairs $ s
    lstPair = last . pairs $ s

completelyContains :: (Int, Int) -> (Int, Int) -> Bool
completelyContains (s1, e1) (s2, e2) = (s1 <= s2 && e1 >= e2) || (s2 <= s1 && e2 >= e1)

main :: IO ()
main = do
  input <- readFile "day4-input.txt"
  let pairs = map parse $ lines input
  let pairsContainedList = map (uncurry completelyContains) pairs
  let totalContains = sum [1 | contains <- pairsContainedList, contains]
  print totalContains
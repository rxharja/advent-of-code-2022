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
completelyContains (x1, y1) (x2, y2) =
  (x1 <= x2 && y1 >= y2)
    || (x2 <= x1 && y2 >= y1)

partiallyContains :: (Int, Int) -> (Int, Int) -> Bool
partiallyContains (x1, x2) (y1, y2) = x2 >= y1 && y2 >= x1

main :: IO ()
main = do
  input <- readFile "day4-input.txt"
  let pairs = map parse $ lines input

  let pairsContainedList = map (uncurry completelyContains) pairs
  let pairsContainedPartiallyList = map (uncurry partiallyContains) pairs

  let totalCompleteContains = sum [1 | contains <- pairsContainedList, contains]
  let totalPartialContains = sum [1 | contains <- pairsContainedPartiallyList, contains]

  putStr "Pairs that are completely contained by the other: "
  print totalCompleteContains

  putStr "Pairs that are partially contained by the other: "
  print totalPartialContains
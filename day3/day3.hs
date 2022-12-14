module Main where

import Data.Maybe (fromMaybe)

priorities :: [(Char, Int)]
priorities =
  [ ('a', 1),
    ('b', 2),
    ('c', 3),
    ('d', 4),
    ('e', 5),
    ('f', 6),
    ('g', 7),
    ('h', 8),
    ('i', 9),
    ('j', 10),
    ('k', 11),
    ('l', 12),
    ('m', 13),
    ('n', 14),
    ('o', 15),
    ('p', 16),
    ('q', 17),
    ('r', 18),
    ('s', 19),
    ('t', 20),
    ('u', 21),
    ('v', 22),
    ('w', 23),
    ('x', 24),
    ('y', 25),
    ('z', 26),
    ('A', 27),
    ('B', 28),
    ('C', 29),
    ('D', 30),
    ('E', 31),
    ('F', 32),
    ('G', 33),
    ('H', 34),
    ('I', 35),
    ('J', 36),
    ('K', 37),
    ('L', 38),
    ('M', 39),
    ('N', 40),
    ('O', 41),
    ('P', 42),
    ('Q', 43),
    ('R', 44),
    ('S', 45),
    ('T', 46),
    ('U', 47),
    ('V', 48),
    ('W', 49),
    ('X', 50),
    ('Y', 51),
    ('Z', 52)
  ]

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

halve :: String -> [String]
halve xs = pairToList $ splitAt (n `div` 2) xs where n = length xs

contains :: Eq a => [a] -> a -> Bool
contains xs x = case xs of
  [] -> False
  v : vs | x == v -> True
  _ : vs -> contains vs x

common :: [String] -> Maybe Char
common xs = do
  let same = foldr common' (head xs) (drop 1 xs)
  if null same then Nothing else Just (head same)

common' :: String -> String -> String
common' sack1 sack2 = [x | x <- sack1, contains sack2 x]

lookup' :: Maybe Char -> Int
lookup' (Just k) = fromMaybe 0 $ lookup k priorities

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative Number or Zero"

main :: IO ()
main = do
  file <- readFile "day3-input.txt"
  let rucksacks = lines file

  let compartments = map halve rucksacks
  let compartmentItems = map common compartments
  let compartmentPriorities = map lookup' compartmentItems
  putStr "Compartment Priorities: "
  print $ sum compartmentPriorities

  let groups = group 3 rucksacks
  let groupItems = map common groups
  let groupPriorities = map lookup' groupItems
  putStr "Group Priorities: "
  print $ sum groupPriorities

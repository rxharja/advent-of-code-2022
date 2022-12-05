-- Day 2: https://adventofcode.com/2022/day/2
module Main where

data Hand = Rock | Paper | Scissors deriving (Eq, Show)

data Result = Unplayed | Success | Draw | Failure deriving (Eq, Show)

data Round = Round Result Hand Int deriving (Eq, Show)

instance Semigroup Round where
  (Round Unplayed Rock i) <> (Round Unplayed Rock _) = Round Draw Rock (i + 3)
  (Round Unplayed Rock i) <> (Round Unplayed Scissors _) = Round Success Rock (i + 6)
  (Round Unplayed Paper i) <> (Round Unplayed Paper _) = Round Draw Paper (i + 3)
  (Round Unplayed Paper i) <> (Round Unplayed Rock _) = Round Success Paper (i + 6)
  (Round Unplayed Scissors i) <> (Round Unplayed Scissors _) = Round Draw Scissors (i + 3)
  (Round Unplayed Scissors i) <> (Round Unplayed Paper _) = Round Success Paper (i + 6)
  (Round Unplayed hand i) <> (Round Unplayed _ _) = Round Failure hand i
  round <> _ = round

-- helper functions
tupleMap :: (a -> b) -> (a, a) -> (b, b)
tupleMap f (a, b) = (f a, f b)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

splitIntoPairs :: String -> (Char, Char)
splitIntoPairs s = (fst, lst)
  where
    fst = head s
    lst = last s

-- domain specific functions
initRound :: Hand -> Int -> Round
initRound = Round Unplayed

initRock :: Round
initRock = initRound Rock 1

initPaper :: Round
initPaper = initRound Paper 2

initScissors :: Round
initScissors = initRound Scissors 3

toRoundPart1 :: Char -> Round
toRoundPart1 c
  | c == 'A' || c == 'X' = initRock
  | c == 'B' || c == 'Y' = initPaper
  | c == 'C' || c == 'Z' = initScissors
  | otherwise = error "Not a valid Input"

toRoundPart2 :: (Char, Char) -> (Round, Round)
toRoundPart2 ('A', 'Z') = (initRock, initPaper)
toRoundPart2 ('B', 'Z') = (initPaper, initScissors)
toRoundPart2 ('C', 'Z') = (initScissors, initRock)
toRoundPart2 ('A', 'Y') = (initRock, initRock)
toRoundPart2 ('B', 'Y') = (initPaper, initPaper)
toRoundPart2 ('C', 'Y') = (initScissors, initScissors)
toRoundPart2 ('A', 'X') = (initRock, initScissors)
toRoundPart2 ('B', 'X') = (initPaper, initRock)
toRoundPart2 ('C', 'X') = (initScissors, initPaper)

score :: Round -> Int
score (Round _ _ i) = i

main :: IO ()
main = do
  input <- readFile "day2-input.txt"
  let handsPart1 = map ((\(a, b) -> (b, a)) . tupleMap toRoundPart1 . splitIntoPairs) . lines $ input
  let handsPart2 = map ((\(a, b) -> (b, a)) . toRoundPart2 . splitIntoPairs) . lines $ input
  let completeRounds = map (uncurry (<>)) handsPart2
  print $ sum . map score $ completeRounds
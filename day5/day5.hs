module Main where

import Control.Monad (foldM)
import Data.Map (Map, adjust, fromList, lookup)
import Data.Maybe (Maybe (..), fromMaybe)
import GHC.Builtin.PrimOps (PrimOp (WhereFromOp))
import Prelude hiding (lookup)

newtype Stack a = Stack [a] deriving (Eq, Show)

newtype Action = Action Int deriving (Eq, Show)

newtype Location = Location Int deriving (Eq, Show)

type Start = Location

type End = Location

type Stacks = Map Int (Stack Char)

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pushMany :: [a] -> Stack a -> Stack a
pushMany xs (Stack xs') = Stack (xs ++ xs')

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x : xs)) = Just (x, Stack xs)

popMany :: Int -> Stack a -> ([a], Stack a)
popMany i (Stack xs) = (take i xs, Stack $ drop i xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x : xs)) = Just x

stacks :: Map Int (Stack Char)
stacks =
  fromList
    [ (1, Stack ['N', 'R', 'J', 'T', 'Z', 'B', 'D', 'F']),
      (2, Stack ['H', 'J', 'N', 'S', 'R']),
      (3, Stack ['Q', 'F', 'Z', 'G', 'J', 'N', 'R', 'C']),
      (4, Stack ['Q', 'T', 'R', 'G', 'N', 'V', 'F']),
      (5, Stack ['F', 'Q', 'T', 'L']),
      (6, Stack ['N', 'G', 'R', 'B', 'Z', 'W', 'C', 'Q']),
      (7, Stack ['M', 'H', 'N', 'S', 'L', 'C', 'F']),
      (8, Stack ['J', 'T', 'M', 'Q', 'N', 'D']),
      (9, Stack ['S', 'G', 'P'])
    ]

parse :: String -> (Action, Start, End)
parse s =
  ( Action $ read (w !! 1),
    Location $ read (w !! 3),
    Location $ read (w !! 5)
  )
  where
    w = words s

replicate' :: Action -> a -> [a]
replicate' (Action i) = replicate i

move :: Start -> End -> Stacks -> Maybe Stacks
move (Location s) (Location e) stacks = do
  (crate, poppedStack) <- lookup s stacks >>= pop
  return $ adjust (push crate) e . adjust (const poppedStack) s $ stacks

moveStack :: Action -> Start -> End -> Stacks -> Maybe Stacks
moveStack action s e stacks = foldM (\s f -> f s) stacks (replicate' action (move s e))

moveStack' :: Action -> Start -> End -> Stacks -> Maybe Stacks
moveStack' (Action i) (Location s) (Location e) stacks = do
  s1 <- lookup s stacks
  let (xs, s1') = popMany i s1
  return $ adjust (pushMany xs) e . adjust (const s1') s $ stacks

operate :: [(Action, Start, End)] -> Stacks -> Maybe Stacks
operate moves stacks = foldM (\stack (a, s, e) -> moveStack a s e stack) stacks moves

operate' :: [(Action, Start, End)] -> Stacks -> Maybe Stacks
operate' moves stacks = foldM (\stack (a, s, e) -> moveStack' a s e stack) stacks moves

popAll :: Maybe Stacks -> Maybe String
popAll stacks = do
  stacks <- stacks
  let locations = map (`lookup` stacks) [1 .. 9]
  let crates = map (>>= peek) locations
  return $ map (fromMaybe ' ') crates

main :: IO ()
main = do
  input <- readFile "day5-input.txt"
  let moves = map parse (lines input)
  let locations = operate moves stacks
  putStr "CrateMover 9000 "
  print $ fromMaybe "Unable to complete operation :(" (popAll locations)

  let locations' = operate' moves stacks
  putStr "CrateMover 9001 "
  print $ fromMaybe "Unable to complete operation :(" (popAll locations')

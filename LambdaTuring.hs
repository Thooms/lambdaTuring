module LambdaTuring where

import Data.Map (Map, (!), lookup, fromList)
import Prelude hiding (Left, Right, lookup)

type State = Int
type Alphabet = [Char]
data Move = Left | Right deriving (Show, Eq)
type Tape = [Char]
data Result = Block | FinalState State deriving (Show, Eq)

data DTuringMachine =
  DTuringMachine {
    states :: [State]
    , inputAlphabet :: Alphabet -- Does not contain the blank character
    , tapeAlphabet :: Alphabet -- Does include the input alphabet and the blank character
    , blank :: Char
    , transitions :: Map (State, Char) (State, Char, Move)
    , initState :: State
    , finalStates :: [State]
    }

run :: DTuringMachine -> Tape -> (Tape, Result)
run m (h:tl) = run_ m [] h (tl ++ repeat (blank m)) (initState m) -- Infinite tape
  where
    run_ m left curr right s -- We use a list zipper for the tape
      | s `elem` finalStates m = (left ++ [curr] ++ right, FinalState s) -- A final state ends the computation
      | otherwise =
          case (s, curr) `lookup` transitions m  of
           Nothing -> (left ++ [curr] ++ right, Block)
           Just (newS, c, move) -> 
             case move of
              Left -> let (l:ls) = left in run_ m ls l (c:right) newS
              Right -> let (r:rs) = right in run_ m (c:left) r rs newS


checkPowerOf2 :: DTuringMachine
checkPowerOf2 =
  DTuringMachine {
    states = [0,1,2]
    , inputAlphabet = "01"
    , tapeAlphabet = "01#"
    , blank = '#'
    , transitions = fromList
      [ ((0, '0'), (0, '0', Right))
      , ((0, '1'), (0, '1', Right))
      , ((0, '#'), (1, '#', Left))
      , ((1, '0'), (2, '0', Left))
      ]
    , initState = 0
    , finalStates = [2]
    }

test1 = let (t, r) = run checkPowerOf2 "111111" in (take 30 t, r)
test2 = let (t, r) = run checkPowerOf2 "10010" in (take 30 t, r)

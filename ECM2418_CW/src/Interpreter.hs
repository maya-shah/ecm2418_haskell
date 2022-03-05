module Interpreter
(
    AExp(N, V, Plus),
    BExp(Bc, Not, And, Less),
    Com (Assign, Seq, If, While, SKIP),
    aval,
    bval,
    eval
) where

import Data.Map (Map, insert)
import qualified Data.Map as Map
import Machine

--Task 2.1
data AExp =
    N Int | V String | Plus AExp AExp
    deriving (Eq, Read, Show)

--Task 2.2
aval :: AExp -> State -> Val
aval (N x) s = x
aval (V v) s = s Map.! v
aval (Plus a1 a2) s = aval a1 s + aval a2 s

--Task 2.1
data BExp =
    Bc Bool | Not BExp | And BExp BExp | Less AExp AExp
    deriving (Eq, Read, Show)

--Task 2.3
bval :: BExp -> State -> Bool
bval (Less a1 a2) s = aval a1 s < aval a2 s
bval (Not b) s = not (bval b s)
bval (And b1 b2) s = bval b2 s && bval b1 s
bval (Bc True) s = True
bval (Bc False) s = False


--Task 2.1
data Com =
    Assign String AExp | Seq Com Com | If BExp Com Com | While BExp Com | SKIP
    deriving (Eq, Read, Show)

--Task 2.4
eval :: Com -> State -> State
eval (Assign v x) s = insert v (aval x s) s
eval SKIP s = s
eval (Seq c1 c2) s = eval c2 (eval c1 s)
eval (If b c1 c2) s = if bval b s then eval c1 s else eval c2 s
eval (While b c) s = if bval b s then eval (While b c) (eval c s) else eval SKIP s
module Machine
(      
        Vname,
        Val,
        State,
        Instr (LOADI, LOAD, ADD, STORE, JMP, JMPLESS, JMPGE),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map (Map, insert)
import qualified Data.Map as Map

--Task 1.1
type Vname = String

--Task 1.2
type Val = Int

--Task 1.3
type State = (Map Vname Val)

--Task 1.4
data Instr = 
        LOADI Int | LOAD String | ADD | STORE String | JMP Int | JMPLESS Int | JMPGE Int
        deriving (Eq, Read, Show)

--Task 1.5
type Stack = [Int]

--Task 1.6
type Counter = Int
type Config = (Counter, State, Stack)

--Task 1.7
iexec :: Instr -> Config -> Config
iexec (LOADI x) (c, s, st) = (c + 1, s, x : st)
iexec (LOAD v) (c, s, st) = (c + 1, s, s Map.! v : st)
iexec ADD (c, s, st) = (c + 1, s, [head st + head (tail st)])
iexec (STORE v) (c, s, st) = (c + 1, insert v (last st) s, drop 1 st)
iexec (JMP i) (c, s, st) = (c + 1 + i, s, st)
iexec (JMPLESS i) (c, s, st) = if head (tail st) < head st then (c + 1 + i, s, drop 2 st) else (c + 1, s, drop 2 st)
iexec (JMPGE i) (c, s, st) = if head (tail st) > head st then (c + 1 + i, s, drop 2 st) else (c + 1, s, drop 2 st)


--Task 1.8
exec :: [Instr] -> Config -> Config
exec [] (c, s, st) = (c, s, st)
exec [n] (c, s, st) = iexec n (c, s, st)
exec (n:ns) (c, s, st)  = exec ns (exec [n] (c, s, st)) 

module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--Task 3.1
acomp :: AExp -> [Instr]
acomp (N x) = [LOADI x]
acomp (V v) = [LOAD v]
acomp (Plus a1 a2) = acomp a1 ++ acomp a2 ++ [ADD]

--Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc b1) b2 i = [JMP i | b1 == b2]
bcomp (Not b) b2 i = bcomp b (not b2) i
bcomp (Less a1 a2) a3 i = acomp a1 ++ acomp a2 ++ if a3 then [JMPLESS i] else [JMPGE i]
bcomp (And b1 b2) b3 i = let bb2 = bcomp b2 b3 i; bb1 = bcomp b1 False (length bb2 + if b3 then 0 else i) in bb1 ++ bb2



--Task 3.3
ccomp :: Com -> [Instr]
ccomp (Seq c1 c2) = ccomp c2 ++ ccomp c1
ccomp (If b c1 c2) = let cc1 = ccomp c1; cc2 = ccomp c2; b1 = bcomp b False (length cc1 + 1) in b1 ++ cc1 ++ [JMP (length cc2)] ++ cc2
ccomp (Assign v x) = acomp x ++ [STORE v]
ccomp (While b c) = let c1 = ccomp c; b1 = bcomp b False (length c1 + 1) in b1 ++ c1 ++ [JMP (-(length b1 + length c1 + 1))]
ccomp SKIP = []
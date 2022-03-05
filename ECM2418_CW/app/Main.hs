module Main where

import System.Environment
import Compiler
import Interpreter

--Task 3.4
main :: IO () 
main = 
    do 
        args <- getArgs
        print (ccomp(read (concat args) :: Com))

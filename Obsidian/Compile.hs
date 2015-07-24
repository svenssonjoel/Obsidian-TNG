{-# LANGUAGE GADTs #-} 
{-# LANGUAGE ScopedTypeVariables #-} 

module Obsidan.Compile where

import Obsidian.Exp
import Obsidian.Shape

import Data.Supply
import Data.List


-- Experimentation with compiling of TNG-Expresions.
-- If Exp can be compiled into something similar
-- to the old Obsidian.CodeGen.Program type, then it can be
-- compiled to CUDA.

-----------------------------------------------------------------
--
-----------------------------------------------------------------

data S = S


class Compilable t where
  compileIt :: Supply Integer -> t -> S 




-----------------------------------------------------------------
-- printing
-----------------------------------------------------------------

printVal :: Value a -> String
printVal (Literal a) = show a
printVal (Variable str) = str
printVal (BinOp op a b) = "(" ++ show op ++ " " ++ printVal a ++ " " ++ printVal b ++ ")"
printVal (Tup2 a b) = "(" ++ printVal a ++", "++ printVal b ++ ")"
printVal (UnLift e) = printExp e
printVal (Index e i) = printExp e ++ " !! " ++ printIx i


printIx :: Index sh -> String 
printIx ix = "(" ++ (unwords $ intersperse "," $ map printVal $ indexL ix) ++ ")" 
  where indexL :: Index sh -> [Value Int] 
        indexL ZI = []
        indexL (SuccI t i) = i : indexL t 

printExp :: Exp p a -> String 
printExp (Lift v) = printVal v
printExp (ParmOp op) = "Parameterised. need a class here"
printExp (RunM m) = "RunM (" ++ printM m ++ ")" 


printM :: M p a -> String
printM (Malloc sh) = "malloc;"
printM (PApply e wf) = "papply"
printM (Assign arr i a) = "assign"
printM (Bind m f) = "bind"
printM (Return a) = "return" 



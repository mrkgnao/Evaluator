module Evaluator (evaluate) where

import Tokenizer
import SYard
import Complex

evaluateRPN list = head $ (foldl evaluate' [] list)

evaluate' :: [Complex] -> String -> [Complex]
evaluate' [] op = [toComplex op]
evaluate' (x:xs) op =
  case op of
    "+"    -> (add x y):ys
    "_"    -> (subtractC y x):ys
    "*"    -> (multiply x y):ys
    "/"    -> (divide y x):ys
    "^"    -> (pow y x):ys
    "-"    -> (negate' x):xs
    "exp"  -> (expC x) :xs
    "ln"   -> (lnC x) :xs
    "sqrt" -> (sqrtC x):xs
    "sin"  -> (sinC x) :xs
    "cos"  -> (cosC x) :xs
    "tan"  -> (tanC x) :xs
    _      -> (toComplex op):x:xs
  where y  = head xs
        ys = tail xs

evaluate :: String -> String
evaluate = toString . cleanUp . evaluateRPN . reverse . toRPN . tokenize
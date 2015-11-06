module Tokenizer (tokenize) where

import Replace
import Complex

tokenize :: String -> [String]
tokenize x = (words . substitute $ x) ++ ["end"]

substitute :: String -> String
substitute x = replaceList k (otherReplacements ++ operatorList ++ functionList)
  where k = impliedAddition (replace x " " "") ""

operatorList = zip operators (map append operators)

functionList = zip functions' (map append functions')

otherReplacements = [("("," ( "),(")"," ) ")]

append :: String -> String
append x = " " ++ x ++ " "

impliedAddition :: String -> String -> String
impliedAddition [] x = x
impliedAddition (x:xs) [] = impliedAddition xs [x]
impliedAddition (x:xs) y
    | isNotOperator (last y) && x == '-' = impliedAddition xs (y ++ "_")
    | otherwise = impliedAddition xs (y ++ [x])
  where isNotOperator k = k `elem` ['0'..'9'] ++ ".)i"

functions' = tail functions
module SYard (toRPN) where

import Data.List
import Data.Char
import Complex

toRPN :: [String] -> [String]
toRPN x = fst (foldl sYard ([],[]) x)

-- first element is output. second one is operator stack.
sYard :: ([a],[a]) -> a -> ([a],[a])
sYard (out,op) x
    | isNumber' x = (x:out, op)
    | isFunction x = (out, x:op)
    | isOperator x = pushOperator (out,op) x
    | x == "(" = (out, x:op)
    | x == ")" = brackets (out,op)
    | x == "end" = popAll (out, op)

pushOperator (out,[]) z = (out,[z])
pushOperator (out,(x:xs)) z
    | criteria z x =  pushOperator ((x:out),xs) z
    | otherwise = (out,(z:x:xs))

popAll (out,[]) = (out,[])
popAll (out,x:op) = popAll (x:out,op)

brackets (out,(x:xs))
    | x /= "(" = brackets (x:out,xs)
    | x == "(" = function (out,xs)

function (out,[]) = (out, [])
function (out,x:xs)
    | isFunction x = (x:out,xs)
    | otherwise = (out,x:xs)

criteria :: String -> String -> Bool
criteria x y
    | y == "(" = False
    | isFunction y = True
    | isLeftAssociative x && getPrecedence x <= getPrecedence y = True
    | (not $ isLeftAssociative x) && getPrecedence x < getPrecedence y = True
    | otherwise = False

getPrecedence :: String -> Int
getPrecedence x = extractFromJust (x `elemIndex` operators)

isLeftAssociative :: String -> Bool
isLeftAssociative "^" = False
isLeftAssociative _ = True

extractFromJust :: Maybe a -> a
extractFromJust (Just a) = a
extractFromJust Nothing = error "Invalid Operator"

isOperator :: String -> Bool
isOperator x = x `elem` operators

isNumber' :: String -> Bool
isNumber' x = and $ map numberChars x
  where numberChars y = isDigit y || y == '.' || y == '-' || y == 'i'

isFunction :: String -> Bool
isFunction x = x `elem` functions  

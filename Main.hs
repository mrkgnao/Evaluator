module Main (main) where

import Evaluator
import Control.Exception (SomeException, catch)

main :: IO ()
main = do
    putStrLn "Math evaluator in Haskell."
    putStrLn "Author: Arya Das"
    putStrLn "-------------------------------------------------"
    putStrLn "Supported functions: exp, ln, sqrt, sin, cos, tan"
    putStrLn "Supported operators: +, -, *, /, ^"
    putStrLn "Operators have precedence."
    putStrLn "Complex numbers are supported in all functions."
    putStrLn "-------------------------------------------------"
    catch iterate' handler
    putStrLn "-------------------------------------------------"
    putStrLn "Thank you for using this calculator."
    

iterate' :: IO ()
iterate' = do
          putStr   ">>> "
          expression <- getLine
          if exitKeywords expression then return ()
          else do
               putStrLn ("    " ++ evaluate expression ++ "\n")
               iterate'

handler :: SomeException -> IO ()
handler e = do
          putStrLn "Error: Could not parse.\n"
          putStrLn "Check   whether   your  expression  is  correct." 
          putStrLn "Also check whether you have used valid functions"
          putStrLn "and  operators.  If  you are sure your that your"
          putStrLn "expression  is  correct,  notify  the developer."
          putStrLn "Github  URL  :   http://www.github.com/aryadas98"
          putStrLn ""
          iterate'

exitKeywords x = x `elem` ["exit","Exit","EXIT","quit","Quit","QUIT"]
module Complex
( Complex, operators, functions, re, im, conjugate, absC, arg, add, subtractC, multiply, divide,
  expC, lnC, pow, sinC, cosC, tanC, toString, toComplex, negate', cleanUp, sqrtC) where

import Data.List
import Replace

data Complex = Complex Float Float deriving Show

operators = ["_","+","/","*","^"]
functions = ["-","exp", "ln", "sin", "cos", "tan","sqrt"]

re :: Complex -> Float
re (Complex x _) = x

im :: Complex -> Float
im (Complex _ y) = y

conjugate :: Complex -> Complex
conjugate (Complex a b) = Complex a (negate b)

absC :: Complex -> Float
absC (Complex a b) = sqrt (a^2 + b^2)

arg :: Complex -> Float
arg (Complex a b) = atan2 b a

negate' :: Complex -> Complex
negate' x = multiply x (Complex (-1) 0)

add :: Complex -> Complex -> Complex
add (Complex a b) (Complex c d) = Complex (a+c) (b+d)

subtractC :: Complex -> Complex -> Complex
subtractC (Complex a b) (Complex c d) = Complex (a-c) (b-d)

multiply :: Complex -> Complex -> Complex
multiply (Complex a b) (Complex c d) = Complex (a*c - b*d) (a*d + b*c)

divide :: Complex -> Complex -> Complex
divide (Complex a b) (Complex c d)
    | s /= 0 = Complex ((a*c + b*d)/s) ((b*c - a*d)/s)
    | otherwise = error "Division by zero!"

  where s = absC (Complex c d) ^ 2

expC :: Complex -> Complex
expC (Complex a b) = Complex (k * cos b) (k * sin b)
  where k = exp a
  
lnC :: Complex -> Complex
lnC (Complex a b) = Complex (log $ absC (Complex a b)) (arg (Complex a b))

pow :: Complex -> Complex -> Complex
pow x y = expC (multiply (lnC x) y)

sqrtC :: Complex -> Complex
sqrtC x = pow x (Complex 0.5 0)

sinC :: Complex -> Complex
sinC (Complex a b) = Complex (sin a * cosh b) (cos a * sinh b)

cosC :: Complex -> Complex
cosC (Complex a b) = Complex (cos a * cosh b) (sin a * sinh b)

tanC :: Complex -> Complex
tanC x = divide (sinC x) (cosC x)

-- A function to round off errors induced by using floating numbers.
-- Here's where I hate the rigorous type system. :-(
cleanUp :: Complex -> Complex
cleanUp (Complex a b)
    | a - m < precision && a - m /= 0 = cleanUp (Complex m b)
    | b - n < precision && b - n /= 0 = cleanUp (Complex a n)
    | o - a < precision && o - a /= 0 = cleanUp (Complex o b)
    | p - b < precision && p - b /= 0 = cleanUp (Complex a p)
    | otherwise = (Complex a b)
  where
    m = fromIntegral (floor a)
    n = fromIntegral (floor b)
    o = fromIntegral (ceiling a)
    p = fromIntegral (ceiling b)
    precision = 10^^(-3)

toString :: Complex -> String
toString (Complex 0 1) = "i"
toString (Complex 0 (-1)) = "-i"
toString (Complex 0 b) = show' b ++ "i"
toString (Complex a 0) = show' a
toString (Complex a b) = show' a ++ show'' b

show' :: Float -> String
show' x
    | x == fromIntegral (floor x) = show (floor x) -- if number is an integer, truncate it.
    | otherwise = show x

show'' :: Float -> String
show'' x
    | x == 1 = "+i"
    | x == -1 = "-i"
    | x > 0 = "+" ++ show' x ++ "i"
    | otherwise = show' x ++ "i"

substitute :: String -> String
substitute x = replaceList x [("+i","+1i") , ("-i","-1i"), ("+"," "), ("-"," -"), ("i"," ")]

tokenize :: String -> [String]
tokenize x = words . substitute $ x

toComplex :: String -> Complex
toComplex x
    | length list == 0 = Complex 0 1 -- number is 0+i.
    | length list == 1 && last x == 'i' = Complex 0 (getHead list) --number is of the form 0+ai.
    | length list == 1 = Complex (getHead list) 0 -- number is of the form a+0i.
    | length list == 2 && last x == 'i' = Complex (getHead list) (getTail list) -- number is of the form a+bi.
    | otherwise = error "Poorly formatted complex number."
  where
    list = tokenize x
    getHead = read . head
    getTail = read . last
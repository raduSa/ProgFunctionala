import Data.List
myInt = 31415926535897932384626433832795028841971693993751058209749445923

double :: Integer -> Integer
double x = x + x

maxim :: Integer -> Integer -> Integer
maxim x y = 
    if (x > y) 
        then x 
    else y

{-max3 x y z = 
    let
        u = maxim x y
    in 
        (maxim  u z)-}

max3 x y z = 
    if (x > y && x > z)
        then x
    else if (y > x && y > z)
        then y
    else z

max4 x y z w = 
    let 
        u = max3 x y z
    in 
        maxim u w

patrate :: Integer -> Integer -> Integer
patrate x y = x * x + y * y

par :: Integer -> String
par x = if mod x 2 == 0 then "par" else "impar"

fact :: Integer -> Integer
fact 1 = 1
fact x = fact(x - 1) * x

big :: Integer -> Integer -> Bool
big x y = if x > y * 2 then True else False

maxList :: [Integer] -> Integer
maxList [x] = x
maxList x = if head x > maxList(tail x) then head x else maxList(tail x)

poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a * (x ^ 2) + b * x + c

eeny :: Integer -> String
eeny x = if even x then "eeny" else "meeny"

fizzbuzz :: Integer -> String
{-fizzbuzz x = 
    if mod x 3 == 0 
        then "fizz"
    else if mod x 5 == 0
        then "buzz"
    else if mod x 3 == 0 && mod x 5 == 0
        then "fizzbuzz"
    else ""-}
fizzbuzz x = 
    let 
        u = if mod x 3 == 0 then "fizz" else ""
        v = if mod x 5 == 0 then "buzz" else ""
    in 
        u ++ v

fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
    
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
    
tribonacciCazuri :: Integer -> Integer
tribonacciCazuri x
    | x <= 2    = 1
    | x == 3    = 2
    | otherwise = tribonacciCazuri (x - 1) + tribonacciCazuri (x - 2) + tribonacciCazuri (x - 3)

tribonacciEcuational :: Integer -> Integer
tribonacciEcuational 1 = 1
tribonacciEcuational 2 = 1
tribonacciEcuational 3 = 2
tribonacciEcuational x = 
    tribonacciEcuational(x-1) + tribonacciEcuational(x-2) + tribonacciEcuational(x-3)

binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k =
    binomial (n-1) k + binomial (n-1) (k-1)

zip' :: [a] -> [b] -> [(a, b)]
zip' [x] [y] = [(x, y)]
zip' l1 [] = []
zip' [] l2 = []
zip' l1 l2 = (head l1, head l2) : zip' (tail l1) (tail l2)

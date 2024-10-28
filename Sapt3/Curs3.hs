import Data.List

(||) :: Bool -> Bool -> Bool
True || x = True
False || x = x

sum' :: [Integer] -> Integer
sum' (x:xs) = x + sum' xs
sum' [] = 0

x = head []
f a = 5


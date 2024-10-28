import Data.Char
sign l = let f x = if (x >0) then 1 else 0 in [f x | x<-l]
squares = map (\x -> x*x)
f = map ($ 3) [(3 +), (2 *), sqrt]
filter' f l = [x | x<-l, f x]
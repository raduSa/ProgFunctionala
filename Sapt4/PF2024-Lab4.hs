{-
[x^2 |x <- [1..10], x `rem` 3 == 2]
[(x,y) | x <- [1..5], y <- [x..(x+2)]]
[(x,y) | x <- [1..3], let k = x^2, y <- [1..k]]
[x | x <- "Facultatea de Matematica si Informatica", elem x ['A'..'Z']]
[[x..y] | x <- [1..5], y <- [1..5], x < y]
-}
factori :: Int -> [Int]
factori n = [x | x<-[1..n], mod n x == 0]

prim :: Int -> Bool
prim n = 
    if length (factori n) == 2
        then True
    else False

numerePrime :: Int -> [Int]
numerePrime n = [x | x<-[2..n], prim x]

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 l1 l2 l3 = (head l1, head l2, head l3) : myzip3 (tail l1) (tail l2) (tail l3)

aplica2 :: (a -> a) -> a -> a
-- aplica2 f x = f (f x)
aplica2 f = f . f
-- aplica2 = \f x -> f (f x)
-- aplica2 f = \x -> f (f x)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]


firstEl :: [(a, b)] -> [a]
firstEl = map fst

sumList :: [[Int]] -> [Int]
sumList = map sum

prel2 :: [Int] -> [Int]
prel2 = map (\x -> if mod x 2 == 0 then div x 2 else x * 2)

inString :: Char -> [String] -> [String]
inString x = filter (elem x)

patrateImpare :: [Int] -> [Int]
patrateImpare = map (^2) . filter odd


patratePozPare :: [Int] -> [Int]
patratePozPare = (map (^2)) . (map fst) . filter (\x -> mod (snd x) 2 == 0) . zip [1..]

numaiVocaleString :: String -> String
numaiVocaleString = filter (`elem` "aeiouAEIOU")

numaiVocale :: [String] -> [String]
numaiVocale = map (filter (`elem` "aeiouAEIOU"))

mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (h:t) = (f h) : mymap f t

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (h:t) = 
    if (f h == True)
        then h : myfilter f t
    else myfilter f t

ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = undefined
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata = undefined

step :: Char -> [[Char]] -> [[[Char]]]
step p c = stepHelper p c 0

stepHelper :: Char -> [[Char]] -> Int -> [[[Char]]]
stepHelper p c i =
    if (i == length c)
        then []
    else
        if (length (c !! i) == 1)
            then stepHelper p c (i + 1)
        else 
            ((take i c) ++ [[p]] ++ (drop (i + 1) c)) : stepHelper p c (i + 1)

next :: Char -> [[[Char]]] -> [[[Char]]]
next p [] = []
next p (h:t) = step p h ++ next p t

{-win :: Char -> [[[Char]]] -> [[[Char]]]
win p l = filter (castigator p) (next p l)

castigator :: Char -> [[Char]] -> Bool
castigator p c =
    if ((c !! 0 == [p] && c !! 1 == [p] && c !! 2 == [p]) ||
        ) -}

import Data.Char
import Data.List

verifL :: [Integer] -> Bool
verifL l1 = 
    if (mod (length l1 ) 2 == 0)
        then True
    else False

takefinal :: [a] -> Int -> [a]
takefinal l1 n = drop (max 0 (length l1 - n)) l1

remove :: [a] -> Int -> [a]
remove l1 n = take (n - 1) l1 ++ (takefinal l1 (length l1 - n))

-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
 | even h    = h `div` 2 : t'
 | otherwise = t'
 where t' = semiPareRec t

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = x : myreplicate (n - 1) x

sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (h:t) =
    if (even h)
        then sumImp t
    else h + sumImp t

totalLen :: [String] -> Int 
-- suma lungimilor stringuri care incep cu 'A'
totalLen [] = 0
totalLen (h:t) = 
    if (head h == 'A')
        then length h + totalLen t
    else totalLen t

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (h:t) = 
    if (h == reverse h)
        then 
            let
                nrVocCuv :: String -> Int
                nrVocCuv [] = 0
                nrVocCuv (h:t) = 
                    if (h `elem` "aeiouAEIOU")
                        then 1 + nrVocCuv t
                    else nrVocCuv t
            in 
                nrVocCuv h + nrVocale t
    else nrVocale t
-- nrVocale ["sos", "civic", "palton", "desen", "aerisirea"] == 9

f :: Int -> [Int] -> [Int]
f _ [] = []
f n (h:t) = 
    if (even h)
        then h : n : f n t
    else h : f n t
-- f 3 [1,2,3,4,5,6] = [1,2,3,3,4,3,5,6,3]

semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]

divizori :: Int -> [Int]
divizori n = [x | x<-[1..n], mod n x == 0]
-- divizori 4 == [1,2,4]

listadiv :: [Int] -> [[Int]]
listadiv l = [divizori x | x <- l]
-- listadiv [1,4,6,8] == [[1],[1,2,4],[1,2,3,6],[1,2,4,8]]

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b l = [x | x<-l, x>=a, x<=b]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = []
inIntervalRec a b (h:t) = 
    if (h>=a && h<=b)
        then h : inIntervalRec a b t
    else inIntervalRec a b t
-- inInterval 5 10 [1..15] == [5,6,7,8,9,10]
-- inInterval 5 10 [1,3,5,2,8,-1] == [5,8]

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t) = 
    if (h > 0)
        then 1 + pozitiveRec t
    else pozitiveRec t

pozitiveComp :: [Int] -> Int
pozitiveComp l = sum [1 | x<-l, x>0]
-- pozitive [0,1,-3,-2,8,-1,6] == 3

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec l = 
    let 
        pozImpAuxRec :: [Int] -> Int -> [Int]
        pozImpAuxRec [] _ = []
        pozImpAuxRec (h:t) i = 
            if (odd h)
                then i : pozImpAuxRec t (i + 1)
            else pozImpAuxRec t (i + 1)
    in
        pozImpAuxRec l 0

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [i | (i, x) <- zip [0..] l, odd x]
-- pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]

multDigitsRec :: String -> Int
multDigitsRec [] = 1
multDigitsRec (h:t) =
    if (isDigit h)
        then digitToInt h * multDigitsRec t
    else multDigitsRec t

multDigitsComp :: String -> Int
multDigitsComp l = product [digitToInt x | x<-l, isDigit x]
-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1

permutari :: [a] -> [[a]]
permutari [x] = [[x]]
permutari l = [ x : y | (i, x) <- zip [1..] l, y <- permutari (remove l i)]

combinari :: [a] -> Int -> [[a]]
combinari l 0 = [[]]
combinari l n = [x : y | (i, x) <- zip [1..] l, y <- combinari (drop i l) (n - 1)]

aranjamente :: [a] -> Int -> [[a]]
aranjamente l 0 = [[]]
aranjamente l n = [x : y | (i, x) <- zip [1..] l, y <- aranjamente (remove l i) (n - 1)]

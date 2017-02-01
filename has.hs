removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b+ c

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY"
lucky x = "Sorry, out of luck"

sayMe :: (Integral a, Show a, Integral b) => a -> b -> String
sayMe 1 _ = "One!"
sayMe 2 _ = "Two!"
sayMe 3 _ = "Three!"
sayMe x _ = (show x)

capital :: String -> String
capital "" = "Empty"
capital all@(x:xs) = "First : " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "a"
    | bmi <= normal = "b"
    | bmi <= fat    = "c"
    | otherwise     = "d"
    where   bmi = weight / height ^ 2
            (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r^2
    in sideArea + 2 * topArea

maximun' :: (Ord a) => [a] -> a
maximun' [] = error "maximun of empty list"
maximun' [x] = x
maximun' (x:xs) = max x (maximun' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs) = x : take' (n-1) xs

--reverse' :: [a] -> [a]
--reverse' [] = []
--reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x


zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

--elem' :: (Eq a) => a -> [a] -> Bool
--elem' a [] = False
--elem' a (x:xs)
--    | a == x = True
--    | otherwise a `elem'` xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallSorted = [a | a <- xs, a <= x]
        biggerSorted = [a | a <- xs, a > x]
    in smallSorted ++ [x] ++ biggerSorted

dropegg :: (Integral a, Ord a) => a -> a -> a
dropegg n k
    | n == 1 || k < 2 = k
    | otherwise = 1 + res
    where tmax x = max (dropegg (n-1) (x-1)) (dropegg (n) (k-x))
          res = minimum [tmax x | x <- [1..k]]

primes = sieve [2..]
    where sieve :: [Int] -> [Int]
          sieve (p:xs) =
            p : sieve [ x | x <- xs, x `mod` p > 0]

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (chains [1..100]))
    where isLong xs = length xs > 15
          chains = map chain

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' =  foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

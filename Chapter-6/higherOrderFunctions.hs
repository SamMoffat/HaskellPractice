multThree :: (Num a) => a -> a -> a -> a 
multThree x y z = x*y*z

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)  --infix work like this as well

isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z']) --infix elem example

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  

fizzbuzz :: (Integral a, Eq a, Show a) => [a] -> [String]
fizzbuzz [] = []
fizzbuzz [x] = (show x) : []
fizzbuzz (x:xs)
  | (x `mod` 15) == 0 = "fizzbuzz" : fizzbuzz xs
  | (x `mod` 3) == 0  = "fizz" : fizzbuzz xs
  | (x `mod` 5) == 0  = "buzz" : fizzbuzz xs
  | otherwise = (show x) : fizzbuzz xs

fizzbuzz' :: (Integral a, Eq a, Show a) => [a] -> [String]
fizzbuzz' xs = [show a | a <- xs, (a `mod` 3) == 0 || (a `mod` 5) == 0 ]

fizzbuzz'' :: (Integral a, Eq a, Show a) => [a] -> [String]
fizzbuzz'' xs = [ if a `mod` 15 == 0 then "FizzBuzz" 
                  else if a `mod` 3 == 0 then "Fizz" 
                  else if a `mod` 5 == 0 then "Buzz" 
                  else show a | a <- xs ]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x  

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y --the two flips are equal

filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []  
filter' p (x:xs)   
  | p x       = x : filter' p xs  
  | otherwise = filter' p xs --guards and functions returning bools go hand-in-hand

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
  let smallerSorted = quicksort (filter (<=x) xs)  
      biggerSorted = quicksort (filter (>x) xs)   
  in  smallerSorted ++ [x] ++ biggerSorted --quicksort example using the higher order filter function

largestDivisible :: (Integral a) => a -> a
largestDivisible x = head (filter p [100000,99999..])
  where p y = y `mod` x == 0

--sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)]) 
--remember takeWhile! good for evaluating upper limit of RESULT not initial val

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
  | even n =  n:chain (n `div` 2)  
  | odd n  =  n:chain (n*3 + 1) 

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
  where isLong xs = length xs > 15  

numLongChains' :: Int  
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))   --lambda function yeah!

sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

sqrtSums :: Float -> Int  
sqrtSums x = length (takeWhile (<x) (scanl1 (+) (map sqrt [1..]))) + 1  --scan works like fold except shows the state of change as an array
                                                                         --that array is being used here to get the total number of mapped sqrts below 1000
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum'' :: Integer
oddSquareSum'' =
  let allOddSquares = filter odd $ map (^2) [1..]
      limit         = takeWhile (<10000)
  in  sum $ limit allOddSquares

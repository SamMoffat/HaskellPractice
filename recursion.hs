maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list!"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

maximum1 :: (Ord a) => [a] -> a
maximum1 [] = error "empty"
maximum1 [x] = x
maximum1 (x:xs) = max x (maximum1 xs)

nSum' :: (Integral a) => [a] -> a
nSum' [] = error "Empty list, stop!"
nSum' [x] = x
nSum' (x:xs) = x + nSum' xs

replicate' :: (Ord i, Num i) => i ->  a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x

take' :: (Ord i, Num i) => i -> [a] -> [a]
take' n _ 
  | n <= 0 = []
take' _ [] = []
take' i (x:xs) = x : take' (i-1) xs
--forst scenario, n and any list, if n is less than or equal to zero, empty list,
--second, 

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys    

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = elem' a xs  

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smallerSorted = quicksort [a | a <- xs, a <= x]
	    biggerSorted  = quicksort [a | a <- xs, a > x]
	in  smallerSorted ++ [x] ++ biggerSorted


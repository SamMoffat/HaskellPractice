first :: (a,b,c) -> a
first (x, _, _) = x

second :: (a,b,c) -> b
second (_, y, _) = y

third :: (a,b,c) -> c 
third (_, _, z) = z

lucky :: (Integral a) => a -> String
lucky 7 = "WINNER WINNER, CHICKEN DINNER."
lucky x = "NOPE."

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * (factorial (n-1))

tell :: (Show a) => [a]-> String
tell [] = "Don't use an empty list!"
tell (x:[]) = "There's only one value in this list and it is " ++ show x
tell (x:y:[]) = "There are two things here and they are " ++ show x ++ " and " ++ show y
tell (x:y:_) = "THAT'S A LOTTA STUFF!"

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "THIS IS EMPTY. STOP IT."
capital all@(x:xs) = "The first entry in " ++ all ++ " is " ++ [x]

bmiTell :: Float -> String
bmiTell bmi
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Average weight"
  | bmi <= 30.0 = "Overweight"
  | otherwise   = "This is not a human's BMI."

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' height weight 
  | bmi <= skinny = "Underweight"
  | bmi <= normal = "Average weight"
  | bmi <= fat    = "Overweight"
  | otherwise     = "This is not a human's BMI."
  where bmi    = height / weight ^ 2
        skinny = 18.5
        normal = 25.0
        fat    = 30.0

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b     = GT
  | a == b    = EQ
  | otherwise = LT

initials :: String -> String -> String
initials first last = [f] ++ ". " ++ [l] ++ ". "
  where (f:_) = first
        (l:_) = last

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi a b | (a, b) <- xs ]
  where bmi a b = a / b ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (h, w) <- xs, let bmi = h / w ^ 2, bmi >=25.0]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea

head' :: [a] -> a
head' [] = error "no items in list!"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of [] -> error "no items in list!"
                       (x:_) -> x

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."

describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
  where what [] = "empty."  
        what [x] = "a singleton list."  
        what xs = "a longer list."                                                 

--cases and lets can be used anywhere, in the middle of a concat there could be 
--    "This string is " ++ case of...
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
  | otherwise     = "This is not a human's BMI." 
    removeNonUppercase :: [Char] -> [Char]  
    removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

    rightTriangle :: Integer -> [(Integer, Integer, Integer)]
    rightTriangle x = [(a,b,c) | c <- [1..x], b <- [1..c], a <- [1..b], a^2+b^2==c^2]

    addThree :: Int -> Int -> Int -> Int
    addThree x y z = x+y+z

    factorial :: Integer -> Integer
    factorial x = product [1..x]

    circumference :: Float -> Float
    circumference r = 2 * pi * r
    circumference' :: Double -> Double
    circumference' r = 2 * pi * r

    addThree' :: (Num a) => a -> a -> a -> a
    addThree' x y z = x+y+z

    
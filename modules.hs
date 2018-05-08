import Data.List  
--import Data.List (nub, sort)  <- select specific functions
--import Data.List hiding (nub)  <- everything except nub

numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub 
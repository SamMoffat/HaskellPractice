<<<<<<< HEAD
import Data.List  
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube
--import Data.List (nub, sort)
--import Data.List hiding (nub)    
--import qualified Data.Map <<< this makes it be refered to like Data.Map.filter so you can still use Prelude.filter
--import qualified Data.Map as M <<< this because M.filter

numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub  

--Prelude.map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]] <<< each inidividual list could describe a polynomial, where the head is of the highest order.
                                                                  --transposing would group all the values of the same order together, sum woul add them together, but only for one list
                                                                  --map would make sum work on each list

--foldl' and foldl1' are stricter versions of foldl and foldl1. They do not lazily calculate the accumulator

--concat <<< takes a 2D list and makes it sequential 1D list. e g concat ["foo","bar","car"]  >>> "foobarcar" 
                                                             --or concat [[3,4,5],[2,3,4],[2,1,1]] >>> [3,4,5,2,3,4,2,1,1]

--concatMap <<< is the same as using the map function then using concat on the produced list e g concatMap (replicate 4) [1..3] >>> [1,1,1,1,2,2,2,2,3,3,3,3]

--and and or <<< boolean logic, and rets true when everything in a list is True, or is when one thing is True

--all and any <<< rets true if any or all Eq generics fit the conditions

--iterate creates an infitate list where first value is the function to perform on each entry and second arg is the starting to the function

--splitAt takes an Int and a list and returns a 2D list with two entries of each new half of the original list before and after the point of split

--takeWhile, similar to take except it takes while an evaluating function returns true. e g takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1] >>> [6,5,4] 

--sort <<< sorts a list. values must be part of Ord typeclass

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
--this function behaces like isInfixOf. Which, like this, returns True if the sublist is somewhere inside the list in question


---TOOK A BREAK FROM LEARNING A SHIT TONNE OF FUNCTIONS

=======
import Data.List  
--import Data.List (nub, sort)  <- select specific functions
--import Data.List hiding (nub)  <- everything except nub

numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub 
>>>>>>> 47b890afccbafee1fd52e2ca81e71a4550ff3071

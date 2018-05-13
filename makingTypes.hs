
import qualified Data.Map as Map
  
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , favouriteFlavour :: String
                     } deriving (Show)
--data Car a b c = Car { company :: a <<<< data types can have parameters (not good in this situation, as all functions would be String, String, Int. )
--                     , model :: b         
--                     , year :: c
--                     }
data Car = Car { company :: String
               , model :: String
               , year :: Int
               }
data Vector a = Vector a a a deriving (Show)
data Guy = Guy { firstname :: String
               , secondName :: String
               } deriving (Eq, Show, Read) --can write Guy == Guy for bool. all attributes must fit within Eq
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum) 

--type PhoneBook = [(String,String)]  --makes PhoneBook a synonym of [(String, String)]
type Code = String  
type LockerMap = Map.Map Int (LockerState, Code)
type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]  --more readable
type AssocList k v = [(k,v)] --synonym types can have parameters as well
--type IntMap v = Map Int v  --make synonym type of multiple functions with params
--type IntMap = Map Int      --or without.
data LockerState = Taken | Free deriving (Show, Eq)  
data TrafficLight = Red | Yellow | Green  

instance Eq TrafficLight where  --instance allows implementation of Eq
   Red == Red = True  
   Green == Green = True  
   Yellow == Yellow = True  
   _ == _ = False 

instance Show TrafficLight where   --implements instance of Show, without this, show wouldn't work for our new self-defiend type.
   show Red = "Red light"  
   show Yellow = "Yellow light"  
   show Green = "Green light" 

--instance Eq (Maybe m) where  --problem here, maybe m has to be equated, but the contents of m doesn't.
--   Just x == Just y = x == y  
--   Nothing == Nothing = True  
--   _ == _ = False

--instance (Eq m) => Eq (Maybe m) where  --this makes the value m as well as Maybe m have to be equatable
--   Just x == Just y = x == y  
--   Nothing == Nothing = True  
--   _ == _ = False 

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

tellCar :: Car -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

vecPlus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector a b c) `vecPlus` (Vector d e f) = Vector (a+d) (b+e) (c+f)

vecMult :: (Num t) => Vector t -> t -> Vector t
(Vector a b c) `vecMult` m = Vector (a*m) (b*m) (c*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector a b c) `scalarMult` (Vector d e f) = a*d + b*e + c*f

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pNo pBook = (name,pNo) `elem` pBook 

lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
   case Map.lookup lockerNumber map of   
      Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
      Just (state, code) -> if state /= Taken   
                              then Right code  
                              else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
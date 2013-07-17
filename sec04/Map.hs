import Data.Char (toUpper)

upperCase :: String -> String

upperCase (x:xs) = toUpper x : upperCase xs
upperCase [] = []


square :: [Double] -> [Double]

square (x:xs) = x*x : square xs
square [] = []

square2 xs = 
  myMap squareOne xs
  where squareOne x = x * x

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : myMap f xs
myMap _ _ = []
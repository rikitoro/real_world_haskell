import Data.List

sufixes :: [a] -> [[a]]
sufixes xs@(_:xs') = xs : sufixes xs'
sufixes _ = []

noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) : noAsPattern xs
noAsPattern _ = []

sufixes2 xs = init (tails xs)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

sufixes3 xs = compose init tails xs

sufixes4 = compose init tails

sufixes5 = init . tails
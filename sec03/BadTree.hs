data Tree a 
  = Node a (Tree a) (Tree a)
  | Empty
  
nodeAreSame (Node a _ _) (Node b _ _)
  | a == b = Just a
nodeAreSame _ _ = Nothing
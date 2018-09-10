-- data type and examples in https://wiki.haskell.org/99_questions/70B_to73
data Tree a = Node a [Tree a]
    deriving (Show, Eq)

tree1 = Node 'a' []
tree2 = Node 'a' [Node 'b' []]
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

-- 70C
nnodes :: Tree a -> Int
nnodes (Node _ xs) = 1 + (sum $ map nnodes xs)

-- 70
stringToTree :: [Char] -> Tree Char
stringToTree s = head . fst $ accum [] s

accum nodes [] = (nodes, [])
accum nodes ('^':ys) = (nodes, ys)
accum nodes (y:ys) = accum (nodes ++ [Node y nodes']) rest'
                      where (nodes', rest') = accum [] ys

-- 71
ipl :: Tree a -> Int
ipl xtr@(Node x ts) = (g xtr - 1) + sum (map ipl ts)
                      where g (Node x []) = 1 
                            g (Node x ts) = 1 + sum (map g ts)

-- 72
bottom_up :: Tree Char -> [Char]
bottom_up (Node x ts) = (foldl (++) [] $ map bottom_up ts) ++ [x]

display_lisp :: Tree Char -> [Char]
display_lisp (Node x []) = x : []
display_lisp (Node x ts) = ('(' : x : ' ' : str') ++ ")"
    where str' = unwords $ map display_lisp ts

--ltl2tree :: [Char] -> Tree Char
ltl2tree s = head . fst $ helper [] s
    where helper nodes [] = (nodes, [])
          helper nodes (' ':xs) = helper nodes xs
          helper nodes ('(':xs) = helper (nodes ++ [Node (head xs) chd]) rest
              where (chd, rest) = helper [] (tail xs)
          helper nodes (')':xs) = (nodes, xs)
          helper nodes (x:xs) = helper (nodes ++ [Node x []]) xs

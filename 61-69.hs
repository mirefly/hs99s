import           Data.List                     as DL

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving Show

-- example tree: https://wiki.haskell.org/99_questions/61_to_69
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)

-- 61
countLeaves :: Tree a -> Int
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ x     y    ) = countLeaves x + countLeaves y

-- 61a
leaves :: Tree a -> [a]
leaves Empty                  = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch _ x     y    ) = leaves x ++ leaves y

-- 62
internals :: Tree a -> [a]
internals Empty                  = []
internals (Branch _ Empty Empty) = []
internals (Branch v x     y    ) = [v] ++ internals x ++ internals y

-- 62b
atLevel :: Tree a -> Int -> [a]
atLevel Empty          _ = []
atLevel (Branch v _ _) 1 = [v]
atLevel (Branch _ x y) n = atLevel x (n - 1) ++ atLevel y (n - 1)

-- 63
xv = 'x'
completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree n = Branch xv (completeBinaryTree l) (completeBinaryTree r)
    where (l, r) = getSubNodes (n - 1) (0, 0)

getSubNodes 0 (l, r) = (l, r)
getSubNodes m (l, r) | l == r    = getSubNodes (m - next) (l + next, r)
                     | otherwise = getSubNodes (m - next) (l, r + next)
    where next = min m (r + 1)

-- Example from https://wiki.haskell.org/99_questions/61_to_69
tree64 = Branch
    'n'
    (Branch
        'k'
        (Branch
            'c'
            (Branch 'a' Empty Empty)
            (Branch 'h' (Branch 'g' (Branch 'e' Empty Empty) Empty) Empty)
        )
        (Branch 'm' Empty Empty)
    )
    (Branch 'u'
            (Branch 'p' Empty (Branch 's' (Branch 'q' Empty Empty) Empty))
            Empty
    )
tree65 = Branch
    'n'
    (Branch
        'k'
        (Branch
            'c'
            (Branch 'a' Empty Empty)
            (Branch 'e' (Branch 'd' Empty Empty) (Branch 'g' Empty Empty))
        )
        (Branch 'm' Empty Empty)
    )
    (Branch 'u' (Branch 'p' Empty (Branch 'q' Empty Empty)) Empty)
-- 64
layout :: Eq a => Tree a -> Tree (a, (Int, Int))
layout Empty = Empty
layout tr    = layout' tr 1
  where
    layout' (Branch v l r) depth = Branch (v, (xpos v, depth))
                                          (layout' l (depth + 1))
                                          (layout' r (depth + 1))
    layout' Empty _ = Empty
    seq = inorderSeq tr
    xpos v' = 1 + (head $ DL.elemIndices v' seq)

inorderSeq Empty          = []
inorderSeq (Branch v l r) = inorderSeq l ++ [v] ++ inorderSeq r

-- 65
layout2 :: Tree a -> Tree (a, (Int, Int))
layout2 Empty = Empty
layout2 tr    = xshift $ helper tr 0 1
  where
    helper (Branch v l r) xpos depth = Branch
        (v, (xpos, depth))
        (helper l (xpos - 2 ^ (maxd - depth - 1)) (depth + 1))
        (helper r (xpos + 2 ^ (maxd - depth - 1)) (depth + 1))
    helper Empty _ _ = Empty
    maxd = maxDepth tr

maxDepth :: Tree a -> Int
maxDepth Empty          = 0
maxDepth (Branch v l r) = 1 + (max (maxDepth l) (maxDepth r))

xshift :: Tree (a, (Int, Int)) -> Tree (a, (Int, Int))
xshift tr = helper tr
  where
    helper (Branch (v, (x, y)) l r) =
        Branch (v, (x + xoff, y)) (helper l) (helper r)
    helper Empty = Empty
    xoff = 1 - (minx tr)
    minx Empty                       = (maxBound :: Int)
    minx (Branch (_, (x', _)) l' r') = min (min x' (minx l')) (minx r')

-- 66
compactTree :: Tree (a, (Int, Int)) -> Tree (a, (Int, Int))
compactTree Empty                               = Empty
compactTree tr@(Branch (v, (x, y)) Empty Empty) = tr
compactTree tr@(Branch (v, (x, y)) l     r    ) = Branch (v, (x, y))
                                                         (shiftTr l' offset)
                                                         (shiftTr r' (-offset))
  where
    minD     = min (maxDepth l) (maxDepth r)
    offset   = (distance - 2) `div` 2
    getx     = fst . snd
    distance = foldl
        min
        (maxDistance tr)
        [ -getx (last (atLevel l' i)) + getx (head (atLevel r' i))
        | i <- [1 .. minD]
        ]
    l' = compactTree l
    r' = compactTree r

maxDistance tr = case tr of
    Branch (_, (x, _)) (Branch (_, (x', _)) _ _) _ -> (x - x') * 2
    Branch (_, (x, _)) _ (Branch (_, (x', _)) _ _) -> (x' - x) * 2
    _ -> 2

shiftTr (Branch (v, (x, y)) l r) dx =
    Branch (v, (x + dx, y)) (shiftTr l dx) (shiftTr r dx)
shiftTr Empty _ = Empty

layout3 tr = xshift $ compactTree $ layout2 tr

-- 67
stringToTree :: String -> Tree Char
stringToTree []       = Empty
stringToTree (x : xs) = Branch x (stringToTree l) (stringToTree r)
    where (l, r) = split' xs

split' :: [Char] -> ([Char], [Char])
split' []         = ([], [])
split' ('(' : xs) = split' $ init xs
split' xs         = (take m xs, drop (m + 1) xs)
  where
    nl = countChar '(' xs
    nr = countChar ')' xs
    m  = length $ takeWhile
        (== False)
        [ (nl !! i == nr !! i) && xs !! i == ',' | i <- [0 .. length xs] ]

countChar :: Char -> [Char] -> [Int]
countChar x s = helper x s 0
  where
    helper x (y : ys) n = if x == y
        then ((n + 1) : (helper x ys (n + 1)))
        else (n : (helper x ys n))
    helper _ [] _ = []

-- 68
preorder :: Tree Char -> [Char]
preorder Empty          = []
preorder (Branch c l r) = [c] ++ preorder l ++ preorder r

inorder :: Tree Char -> [Char]
inorder Empty          = []
inorder (Branch c l r) = inorder l ++ [c] ++ inorder r

preInTree :: [Char] -> [Char] -> Tree Char
preInTree preord inord = helper prein
  where
    prein = [ (p, head (DL.elemIndices p inord)) | p <- preord ]
    n     = length preord
    helper []              = Empty
    helper ((x1, x2) : zs) = Branch
        x1
        (helper $ takeWhile (\z -> snd z < x2) zs)
        (helper $ dropWhile (\z -> snd z < x2) zs)

-- 69
tree2ds Empty          = ['.']
tree2ds (Branch c l r) = [c] ++ tree2ds l ++ tree2ds r

ds2Tree ('.' : xs) = (Empty, xs)
ds2Tree (x   : xs) = (Branch x l r, re')
  where
    (l, re ) = ds2Tree xs
    (r, re') = ds2Tree re

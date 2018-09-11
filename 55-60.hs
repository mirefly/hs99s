data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

-- 55
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n
    | (odd n) = [Branch 'x' b1 b2 | b1 <- bs, b2 <- bs]
    | otherwise = [Branch 'x' b1 b2 | b1 <- bs, b2 <- bs']
                   ++ [Branch 'x' b1 b2 | b1 <- bs', b2 <- bs ]
    where bs = cbalTree (n `div` 2)
          bs' = cbalTree (n `div` 2 - 1)

-- 56
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r)= mirror l r

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l r) (Branch _ l' r') = (mirror l r') && (mirror r l')
mirror _ _ = False

-- 57
construct :: [Int] -> Tree Int
construct ns = foldl (\r n -> add n r) Empty ns
            where add x (Branch v l r)
                    | x < v = Branch v (add x l) r
                    | x >= v = Branch v l (add x r)
                  add x Empty = Branch x Empty Empty

-- 58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric $ cbalTree n

-- 59
hbalTree :: Char -> Int -> [Tree Char]
hbalTree v 1 = [Branch v Empty Empty]
hbalTree v 2 = [(Branch v Empty leaf), (Branch v leaf Empty), (Branch v leaf leaf)]
            where leaf = Branch v Empty Empty
hbalTree v h = [Branch v x' x | x' <- ts', x <- ts]
                ++ [Branch v x x' | x' <- ts', x <- ts]
                ++ [Branch v x y | x <- ts, y <- ts]
                where ts = hbalTree v (h-1)
                      ts' = hbalTree v (h-2)

-- 60
maxNodes :: Int -> Int
maxNodes h = 2^h-1

minNodes :: Int -> Int
minNodes h 
    | h <= 0 = 0
    | h == 1 = 1
    | otherwise = 1 + minNodes (h-1) + minNodes (h-2)

maxHeight :: Int -> Int
maxHeight n = length $ takeWhile (<= n) $ map minNodes [1..] 

height :: Tree a -> Int
height Empty = 0
height (Branch _ l r) = 1 + (max (height l) (height r))

hbalTreeNodes :: Char -> Int -> [Tree Char]
hbalTreeNodes v 0 = [Empty]
hbalTreeNodes v 1 = [Branch v Empty Empty]
hbalTreeNodes v n = concat [build lm (n-1-lm) | lm <- lms]
    where build m m'= [Branch v l r | l <- hbalTreeNodes v m,
                                      r <- hbalTreeNodes v m',
                                      abs (height l - height r) <=1 ]
          lmin = minNodes ((maxHeight n) - 2)
          
          lms = [lmin..((n-1)-lmin)]

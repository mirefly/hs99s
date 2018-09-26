-- 1
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast [x] = x
myLast (x:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [] = error "Too short list"
myButLast [x] = error "Too short list"
myButLast [x0, x1] = x0
myButLast (x0:xs) = myButLast xs

-- 3
-- not work for infinite lists
elementAt :: [a] -> Int -> a
elementAt xs n =
    if (n > length xs) || n <=0  then error "invalid index"
    else xs !! (n-1)

-- 3
elementAt' :: [a] -> Int -> a
elementAt' [] n = error "invalid index"
elementAt' (x:xs) n
    | n < 1     = error "invalid index"
    | n == 1    = x
    | otherwise = elementAt' xs (n-1)

-- 4
myLength :: [a] -> Int
myLength xs = foldr (\ x -> (+) 1) 0 xs

-- 5
-- repeatedly reconses, wasteful
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 5
myReverse' :: [a] -> [a]
myReverse' xs = foldl f [] xs
    where f ys x = x:ys

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List ys) = foldl f [] ys
    where f r y = r ++ flatten y

-- 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress xs = foldr f [last xs] xs
    where f x acc = if head acc == x then acc
                    else x:acc

-- 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = foldr f [[]] xs
    where f x (r:rs)
            | null r = [[x]]
            | head r == x = (x:r):rs
            | otherwise = [x]:(r:rs)

-- 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

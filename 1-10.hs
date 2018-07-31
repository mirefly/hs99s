-- 1
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- 2
myButLast :: [a] -> a
myButLast [] = error "Too short list"
myButLast (x:[]) = error "Too short list"
myButLast (x0:(x1:[])) = x0
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
    | otherwise = elementAt' (xs) (n-1)

-- 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 5
-- repeatedly reconses, wasteful
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- 5
myReverse' :: [a] -> [a]
myReverse' xs = foldl f [] xs
    where f ys x = (x:ys)


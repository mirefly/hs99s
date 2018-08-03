import Data.List as DL

-- 11
data Item a = Single a | Multiple Int a
    deriving Show

encodeModified :: Eq a => [a] -> [Item a]
encodeModified xs = map f (DL.group xs)
    where f (y:[]) = Single y
          f ys = Multiple (length ys) (head ys)

-- 12
decodeModified :: [Item a] -> [a]
decodeModified xs = foldl (++) [] (map f xs)
    where f (Single y) = [y]
          f (Multiple n y) = replicate n y

-- 13
encodeDirect :: Eq a => [a] -> [Item a]
encodeDirect [] = []
encodeDirect xss@(x:xs) =
    let n = count xss
        r = drop n xss
    in case count xss of
        0 -> []
        1 -> [Single x] ++ (encodeDirect r)
        _ -> [Multiple n x] ++ (encodeDirect r)

count :: Eq a => [a] -> Int
count [] = 0
count [y] = 1
count (y:ys) = if (y)==(head ys) then (1+(count ys)) else 1

-- 14
dupli :: [a] -> [a]
dupli xs = foldr (\x res -> x:x:res) [] xs

-- 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> take n $ repeat x) xs

-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n
    | length xs < n = xs
    | otherwise = (init $ take n xs) ++ (dropEvery (drop n xs) n)

-- 17 -- predefined predicates
split :: [a] -> Int -> ([a], [a])
split xs n = ((take n xs), (drop n xs))

-- 17
split' :: [a] -> Int -> ([a], [a])
split' [] n | n>0 = ([], [])
split' xs n | n<=0  = ([], xs)
split' (x:xs) n = ([x]++(fst s'), snd s')
                  where s' = split' xs (n-1)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice xs n1 n2 = [snd x | x <- filter f $zip [1..] xs]
                where f x = (fst x <= n2) && (fst x >= n1)

-- 19
rotate :: [a] -> Int -> [a]
rotate xs n
    | n >= 0 = (drop n xs) ++ (take n xs)
    | n <0 = reverse $ rotate (reverse xs) (-n)

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs
    | n <= 0 || n > length xs = error "incorrect index"
    | otherwise = (xs !! (n-1), (take (n-1) xs) ++ (drop n xs))

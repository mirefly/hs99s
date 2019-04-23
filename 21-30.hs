import           System.Random                 as SR
import           Data.List                     as DL

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt y xs n = let r = splitAt (n - 1) xs in fst r ++ (y : snd r)

-- 22
range :: Int -> Int -> [Int]
range a b = take (b - a + 1) [a ..]

-- 23
-- may get same letters
rnd_select :: String -> Int -> IO String
rnd_select s n = do
    g <- SR.getStdGen
    let ks = take n $ SR.randomRs (0, length s - 1) g
    return $ map (\k -> s !! k) ks

-- 24
diff_select :: Int -> Int -> IO [Int]
diff_select n xm = do
    y <- select_n n [1 .. xm]
    return (fst y)

select_n :: Int -> [a] -> IO ([a], [a])
select_n 0 xs = return ([], xs)
select_n n xs = do
    g <- SR.getStdGen
    let k = fst $ SR.randomR (0, length xs - 1) g
    y' <- select_n (n - 1) (take k xs ++ drop (k + 1) xs)
    return ((xs !! k) : fst y', snd y')

-- 25
rnd_permu :: String -> IO String
rnd_permu s = do
    let len = length s
    xs <- diff_select len len
    return $ map (\x -> s !! (x - 1)) xs

-- 26
combinations :: Int -> [a] -> [[a]]
combinations n [] | n == 0    = [[]]
                  | otherwise = []
combinations n xss@(x : xs)
    | n > length xss
    = []
    | n == length xss
    = [xss]
    | otherwise
    = map (\v -> x : v) (combinations (n - 1) xs) ++ combinations n xs

-- 27
{-
permutation :: [a] -> [[a]]
permutation [x] = [[x]]
permutation xs = foldl (++) [] $ map f $ zip xs [0..]
    where f xy = let x = fst xy
                     y = snd xy
                     re = (take y xs) ++ (drop (y+1) xs)
                 in map (\v->(x:v)) $ permutation re
-}

group' :: Eq a => [Int] -> [a] -> [[[a]]]
group' [n     ] xs = map ((: [])) $ combinations n xs
group' (n : ns) xs = concat $ map f $ combinations n xs
    where f ys = map ((:) ys) $ group' ns $ filter (\v -> notElem v ys) xs

-- 28
lsort :: [[a]] -> [[a]]
lsort []       = []
lsort (x : xs) = lsort short ++ [x] ++ lsort long
  where
    short = [ y | y <- xs, length y <= length x ]
    long  = [ y | y <- xs, length y > length x ]

lfsort :: [[a]] -> [[a]]
lfsort xs = concat $ DL.sortBy comparelen $ DL.groupBy beEq $ DL.sortBy
    comparelen
    xs
  where
    comparelen = \x y -> compare (length x) (length y)
    beEq z1 z2 = (==) EQ $ comparelen z1 z2

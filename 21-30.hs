-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt y xs n = let r = splitAt (n-1) xs in (fst r) ++ (y:(snd r))

-- 22
range :: Int -> Int -> [Int]
range a b = take (b-a+1) [a..]

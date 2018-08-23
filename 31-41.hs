-- 31
isPrime :: Int -> Bool
isPrime x
    | x <= 1 = False
    | otherwise = all (/=0) $ map (mod x) [2..floor ((fromIntegral (x))/2)]

-- 32 Euclidean algorithm
myGCD :: Int -> Int -> Int
myGCD x y = gcd' (abs x) (abs y)
    where gcd' x' y' = if x' == 0 then y' else gcd' (mod y' x') x'

-- 33
coprime :: Int -> Int -> Bool
coprime x y = myGCD x y == 1

-- 34
totient :: Int -> Int
totient 1 = 1
totient x = foldl (\r xi -> if xi == True then (r + 1) else r) 0
            $ map (coprime x) [1..x-1]

-- 35
primeFactors :: Int -> [Int]
primeFactors x = reverse $ getFactors x [2]
                where  getFactors m (f:fs)
                            | f*f > m = m:fs
                            | mod m f == 0 = getFactors (div m f) (f:f:fs)
                            | otherwise = getFactors m ((f+1):fs)

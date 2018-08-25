import Data.List as DL
import System.CPUTime as SC
import Text.Printf as TP

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

-- 36
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult x = map (\x-> (head x, length x)) $ group $ primeFactors x

-- 37
phi :: Int -> Int
phi x = product $ map (\f -> let f' = head f in (f'-1)*f'^((length f)-1)) fl
    where fl = DL.group $ primeFactors x

-- 38
timef :: (Int -> Int) -> Int -> IO ()
timef f a = do
    start <- SC.getCPUTime
    f a `seq` return ()
    end <- SC.getCPUTime
    putStrLn $ show $ (fromIntegral (start-end)) / (10^12)

comparePhi :: Int -> IO ()
comparePhi x = do
    timef totient x
    timef phi x

-- 39
primeR :: Int -> Int -> [Int]
primeR lower upper = filter isPrime [lower..upper]

-- 40
goldbach :: Int -> (Int, Int)
goldbach x = (x1, x-x1)
    where x1 = head $ filter (\y-> isPrime (x-y)) $ primeR 2 x

-- 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList lower upper = map goldbach $ filter even [lower..upper]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' lower upper lim = filter f $ goldbachList lower upper
    where f y = fst y > lim && snd y > lim

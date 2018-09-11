-- 46, 47
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False


or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True  = True
or' _ _ = False

nand' :: Bool -> Bool -> Bool 
nand' x y = not $ and' x y

nor' :: Bool -> Bool -> Bool 
nor' x y = not $ or' x y

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False  = True
equ' _ _ = False

-- Material conditional (material implication)
impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

toString :: [Bool] -> String
toString [] = []
toString [x] = show x
toString (x:xs) = show x ++ " " ++ toString xs

table :: (Bool -> Bool -> Bool) -> IO ()
table f = do
    helper [True, True]
    helper [True, False]
    helper [False, True]
    helper [False, False]
    where helper [x, y] = putStrLn $ toString [x, y, f x y]

-- 48
toString' :: [[Bool]] -> String
toString' [] = ""
toString' [x] = toString x
toString' (x:xs) = toString x ++ "\n" ++ toString' xs

combinationsn :: Int -> [[Bool]]
combinationsn 1 = [[True], [False]]
combinationsn n =  map ((:) True) c' ++ map ((:) False) c'
    where c' = combinationsn (n-1)
    
-- from https://wiki.haskell.org/99_questions/46_to_50
infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 7 `equ'`
--infixl 3 `equ'`
---

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = do
    putStrLn $ toString' $ map (\x -> x ++ [f x]) $ combinationsn n

-- 49
grey :: Int -> [String]
grey 1 = ["0", "1"]
grey x = (map ((:) '0') y') ++ (map ((:) '1') $ reverse y')
    where y' = grey (x-1)

-- 50

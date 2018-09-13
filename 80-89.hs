data Graph = Graph [Char] [(Char, Char)]
    deriving Show
data Adj = Adj [(Char, [Char])]
    deriving Show

-- 80 Not completed
graphToAdj :: Graph -> Adj
graphToAdj (Graph nodes edges) = Adj [(x, neighbors x) | x <- nodes]
    where neighbors x = [ if fst e == x then snd e else fst e | e <- edges,
                          fst e == x || snd e == x]

adjToGraph :: Adj -> Graph
adjToGraph (Adj xs) = Graph nodes edges
    where nodes = map fst xs
          edges = cleanEdgeList [(x, y) | (x, ys) <- xs, y <- ys]

cleanEdgeList :: (Ord a) => [(a, a)] -> [(a, a)]
cleanEdgeList xs = rmdup [if x > x' then (x', x) else (x, x')| (x, x') <- xs]

rmdup :: Eq a => [a] -> [a]
rmdup ys = foldl (\res y -> if y `elem` res then res else res++[y]) [] ys

-- 81 directed graph
paths :: Int -> Int -> [(Int, Int)] -> [[Int]]
paths s d edges = helper [[s]] []
    where helper [] final = final
          helper possible final = 
                helper (filter (not . f) possible') (final++(filter f possible')) where possible' = concat $ map extend possible
                      f = \x->last x == d
          extend steps =  [steps ++ [p] |  p <- (neighbors (last steps)),
                           not (p `elem` steps)]
          neighbors p = [p2 | (p1, p2) <- edges, p1 == p ]

cycle' :: Int -> [(Int, Int)] -> [[Int]]
cycle' sd edges = helper [[sd]] []
    where helper [] final = final
          helper possible final = 
                helper (filter (not . f) possible') (final++(filter f possible'))
                where possible' = concat $ map extend possible
                      f = \x -> (last x == sd) && length x > 1
          extend steps =  [steps ++ [p] |  p <- (neighbors (last steps)),
                           not (p `elem` (tail steps))]
          neighbors p = [p2 | (p1, p2) <- edges, p1 == p ]

-- spanning trees

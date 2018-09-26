import Data.List

data Graph = Graph String [(Char, Char)]
            deriving (Show, Eq)
data Adj = Adj [(Char, String)]
          deriving Show

--80 Not completed
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

--81 directed graph
paths :: Int -> Int -> [(Int, Int)] -> [[Int]]
paths s d edges = helper [[s]] []
    where helper [] final = final
          helper possible final = helper (filter (not . f) possible') (final++filter f possible')
            where possible' = concatMap extend possible
                  f = \x->last x == d
                  extend steps =  [steps ++ [p] |  p <- neighbors (last steps),
                              p `notElem` steps]
                  neighbors p = [p2 | (p1, p2) <- edges, p1 == p ]

--82
cycle' :: Int -> [(Int, Int)] -> [[Int]]
cycle' sd edges = helper [[sd]] []
    where helper [] final = final
          helper possible final =
              helper (filter (not . f) possible') (final++filter f possible')
                where possible' = concatMap extend possible
                      f = \x -> (last x == sd) && length x > 1
                      extend steps =  [steps ++ [p] |  p <- neighbors (last steps),
                                        p `notElem` (tail steps)]
                      neighbors p = [p2 | (p1, p2) <- edges, p1 == p ]

--83 spanning trees
--k4 from https://wiki.haskell.org/99_questions/Solutions/83

k4 = Graph ['a', 'b', 'c', 'd'] [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]
--
expandTree :: (Graph, Graph) -> [(Graph, Graph)]
expandTree g@(Graph cs es, Graph cs' es')
    | length cs == length cs' = [g]
    | otherwise = map f [1..length next_es]
          where (next_es, others) = sepConn cs' (fst g)
                pointNotIn (ep1, ep2) = if ep1 `elem` cs' then ep2 else ep1
                f i = (Graph cs (drop i next_es++others),
                    Graph (pointNotIn (next_es!!(i-1)):cs') ((next_es!!(i-1)):es'))

sepConn :: String -> Graph -> ([(Char, Char)], [(Char, Char)])
sepConn cs (Graph _ es) = ([ (a, b) | (a, b)<-es, f a b || f b a]
                         , [ (a, b) | (a, b)<-es, not (g a), not (g b)])
                            where f a b = g a && not (g b)
                                  g a = a `elem` cs

spantree :: Graph -> [(Graph, Graph)]
spantree g@(Graph ns es) =  helper initial
    where initial = [(g, Graph [head ns] [])]
          helper xs = if xs == ys then xs else helper ys
            where ys = concatMap expandTree xs

--84 Prim algorithm, minimal spanning tree

data Graph' = Graph' [Int] [(Int, Int, Int)]

extend :: Graph' -> Graph' -> Graph'
extend (Graph' xs xes) (Graph' ys yes) = Graph' (nexty:ys) (nextye:yes)
    where isConn m n = (m `elem` ys) && notElem n ys
          nextye = minEdge [ xe | xe@(a, b, _) <- xes, isConn a b || isConn b a]
          nexty = let (a, b, _)  = nextye
                  in if a `elem` ys then b else a

minEdge :: [(Int, Int, Int)] -> (Int, Int, Int)
minEdge xs = foldl1 f xs
    where f r@(_,_,rw) l@(_,_,lw) = if rw<=lw then r else l

prim :: [Int] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
prim xs ys = let Graph' _ out = helper (Graph' [head xs] []) in out
    where helper g'@(Graph' xs' ys') = if length xs' == length xs then g'
                                       else helper (extend g g')
          g = Graph' xs ys

--85 brutal
--require input nodes labels are [1..]
combinations :: [Int] -> [[Int]]
combinations [x] = [[x]]
combinations xs = [ x':xs' | x'<-xs, xs'<-combinations (filter (/= x') xs)]

data GraphI = GraphI [Int] [(Int, Int)]
             deriving (Show, Eq)

mapEdges :: [(Int, Int)] -> [Int] -> [(Int, Int)]
mapEdges xs ms = sort [ (ms!!(a-1), ms!!(b-1)) | (a, b) <- xs]

iso :: GraphI -> GraphI -> Bool
iso (GraphI xs xes) (GraphI ys yes) = (sort xs == sort ys) &&
    elem (sort yes)  (map (mapEdges xes) (combinations xs))

--example from https://wiki.haskell.org/99_questions/80_to_89
graphG1 = GraphI [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = GraphI [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]

-- 86 Welch-Powell's algorithm

kcolor :: String -> [(Char, Char)] -> [(Char, Int)]
kcolor xs ys = sort $ helper [] xs'
    where colors = [1..]
          xs' = sortByDegDec xs ys
          helper cs [] = cs
          helper cs (z:zs) = helper ((z, head [c|c<-colors,
                    c `notElem` neighborsColors z ys cs]):cs) zs

sortByDegDec :: String -> [(Char, Char)] -> String
sortByDegDec xs ys = sortBy (\x y -> compare (degree x) (degree y)) xs
    where degree c = length $ filter (\y -> c == fst y || c == snd y) ys

neighborsColors :: Char -> [(Char, Char)] -> [(Char, Int)] -> [Int]
neighborsColors x ys cs = [snd c | c<-cs, fst c `elem` colorNeighbors]
    where neighbors = [if x == fst y then snd y else fst y | y <- ys,
                        x == fst y || x == snd y]
          colorNeighbors = [n | n <- neighbors, n `elem` fst (unzip cs)]

-- 87
depthFirst :: ([Int], [(Int, Int)]) -> Int -> [Int]
depthFirst (xs, ys) z = reverse $ update [z] [z]
    where neighbors c = [if fst y == c then snd y else fst y
                         | y<-ys, fst y == c || snd y == c]
          update visited [] = visited
          update visited s@(top:stack) =
            if null ms then update visited stack
            else  let m' = head ms in update (m':visited) (m':s)
                where ms = [m | m <- neighbors top, m `notElem` visited]

-- 88
connectedComponents :: ([Int], [(Int, Int)]) -> [[Int]]
connectedComponents xys@([], ys) = []
connectedComponents xys@(xs, ys) = newgr : connectedComponents (xs', ys)
        where newgr = depthFirst xys (head xs)
              xs' = [x | x<-xs, x `notElem` newgr]

-- 89 -- brute force, enumerate all decomposings, limited to node labels [1..]
bipartite :: ([Int], [(Int, Int)]) -> Bool
bipartite (xs, ys) = any f ms
    where ms = enumBool (length xs)
          f m = all (\(a, b) -> m!!(a-1) /= m!!(b-1)) ys

enumBool :: Int -> [[Bool]]
enumBool 1 = [[True], [False]]
enumBool n = let last = enumBool (n-1) in map (True :) last ++ map (False :) last

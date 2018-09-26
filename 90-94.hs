-- 90
queens :: Int -> [[Int]]
queens n = foldl (\res _ -> concatMap next res) [[]] [1..n]
    where next [] = map (: []) [1..n]
          next ys = [ys ++ [new] | new <- [1..n],
            notElem new ys && notElem new diag ys (length ys + 1)]
          diag ys m = concat $ zipWith (\y x -> [y-x, y+x]) ys (reverse [1..(m-1)])


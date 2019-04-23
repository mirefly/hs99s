-- 90
queens :: Int -> [[Int]]
queens n = foldl (\res _ -> concatMap next res) [[]] [1 .. n]
  where
    next [] = map (: []) [1 .. n]
    next ys =
        [ ys ++ [new]
        | new <- [1 .. n]
        , notElem new ys && notElem new (diag ys (length ys + 1))
        ]
    diag ys m = concat
        $ zipWith (\y x -> [y - x, y + x]) ys (reverse [1 .. (m - 1)])

-- 91 brute, cannot solve 8x8 board --- TODO
knightTo :: Int -> (Int, Int) -> [[(Int, Int)]]
knightTo m (x, y) = helper [[(x, y)]]
  where
    m2 = m * m
    helper :: [[(Int, Int)]] -> [[(Int, Int)]]
    helper [] = []
    helper xyss
        | length (head xyss) == m2 = xyss
        | otherwise = helper $ concatMap
            (\xys@(xy : _) -> map (: xys) $ filter (`notElem` xys) $ next xy)
            xyss
    next :: (Int, Int) -> [(Int, Int)]
    next (x, y) = filter
        inboard
        [ (x + dx, y + dy)
        | dx <- [2, 1, -1, -2]
        , dy <- [3 - abs dx, abs dx - 3]
        ]
    inboard (x, y) = if x > 0 && x <= m && y > 0 && y <= m then True else False

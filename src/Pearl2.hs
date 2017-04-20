module Pearl2 where

import Criterion.Main

-- P := people at the party
-- C ⊆ P
-- Everybody knows every member of C
-- Members of C only know each other
-- Find C!

knows :: Integer -> Integer -> Bool
knows x y = case (x, y) of
              (x, y) | x == y -> True -- everyone knows themself (won't work otherwise)
              (_, 1) -> True  -- in clique
              (_, 3) -> True  -- in clique
              (1, _) -> False -- diss!
              (3, _) -> False -- diss!
              _      -> True  -- everyone else can know each other, they're still not part of the clique



-- 1. Exponential Time Solution

-- | Checks if 'cs' is a clique of celebreties in 'ps'.
cs <§ ps = and [pred p c | p <- ps, c <- cs]
  where pred p c = p `knows` c && (if p `elem` cs then c `knows` p else True)

subseqs :: [a] -> [[a]]
subseqs [] = [[]]
subseqs (x : xs) = map (x:) (subseqs xs) ++ subseqs xs

-- subseqs [True, False] == [[True,False],[True],[False],[]]
-- subseqs [1,2,3] == [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]] Weird order?

cclique1 :: Integer -> Maybe [Integer]
cclique1 n = let ps = [1..n]
           in case (filter (<§ ps) (subseqs ps)) of
                []    -> Nothing
                (x:_) -> Just x



-- 2. Quadratic Time Solution

nonmember p cs = and [p `knows` c && not (c `knows` p) | c <- cs]
member p ps cs = and [x `knows` p && (if x `elem` cs then p `knows` x else True) | x <- ps]

ccliques :: [Integer] -> [[Integer]]
ccliques [] = [[]]
ccliques (p : ps) = map (p:) (filter (member p ps) css) ++
                    filter (nonmember p) css
                    where css = ccliques ps

cclique2 :: Integer -> [Integer]
cclique2 n = (head . ccliques) [1..n]



-- 3. Linear Time Solution

cclique3 :: Integer -> [Integer]
cclique3 n = foldr op [] [1..n]

op p cs | null cs = [p]
        | not (p `knows` c) = [p]
        | not (c `knows` p) = cs
        | otherwise = p : cs
        where c = head cs



-- 4. Fusion / Confusion

subseqs' :: [a] -> ([a], [[a]])
subseqs' = foldr step ([], [[]])

step :: a -> ([a], [[a]]) -> ([a], [[a]])
step x (xs, xss) = (x : xs, map (x:) xss ++ xss)

cclique4 :: Integer -> [Integer]
cclique4 n = let ps = [1..n]
                 f (ps, css) = head (filter (<§ ps) css)
             in (f . subseqs') ps



-- Profiling

pearl2 :: IO ()
pearl2 = do
  let n = 25  
  defaultMain [
    bgroup "pearl2" [
        bench "cclique1" $ whnf cclique1 n,
        bench "cclique2" $ whnf cclique2 n,
        bench "cclique3" $ whnf cclique3 n,
        bench "cclique4" $ whnf cclique4 n
        ]
    ]

{- benchmarking pearl2/cclique1
   time                 2.529 s    (2.386 s .. 2.727 s)
                        0.999 R²   (0.998 R² .. 1.000 R²)
   mean                 2.492 s    (2.458 s .. 2.514 s)
   std dev              33.65 ms   (0.0 s .. 38.83 ms)
   variance introduced by outliers: 19% (moderately inflated)
   
   benchmarking pearl2/cclique2
   time                 342.6 μs   (342.0 μs .. 343.1 μs)
                        1.000 R²   (1.000 R² .. 1.000 R²)
   mean                 343.0 μs   (342.6 μs .. 344.0 μs)
   std dev              2.135 μs   (1.283 μs .. 3.786 μs)
   
   benchmarking pearl2/cclique3
   time                 1.408 μs   (1.402 μs .. 1.418 μs)
                        1.000 R²   (1.000 R² .. 1.000 R²)
   mean                 1.406 μs   (1.404 μs .. 1.412 μs)
   std dev              12.12 ns   (5.813 ns .. 19.08 ns)
   
   benchmarking pearl2/cclique4
   time                 2.428 s    (1.378 s .. 2.973 s)
                        0.979 R²   (0.937 R² .. NaN R²)
   mean                 2.483 s    (2.327 s .. 2.589 s)
   std dev              158.3 ms   (0.0 s .. 182.8 ms)
   variance introduced by outliers: 19% (moderately inflated)
   
   stack exec Pearls  89.08s user 3.64s system 99% cpu 1:32.90 total -}

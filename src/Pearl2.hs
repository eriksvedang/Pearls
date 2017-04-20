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
  defaultMain [
    bgroup "pearl2" [
        bench "cclique1 20" $ whnf cclique1 20,
        bench "cclique2 20" $ whnf cclique2 20,
        bench "cclique3 20" $ whnf cclique3 20,
        bench "cclique2 50" $ whnf cclique2 50,
        bench "cclique3 50" $ whnf cclique3 50
        --bench "cclique4" $ whnf cclique4 n
        ]
    ]

{-
benchmarking pearl2/cclique1 20
time                 65.81 ms   (61.33 ms .. 69.32 ms)
                     0.995 R²   (0.991 R² .. 0.999 R²)
mean                 68.43 ms   (66.54 ms .. 73.34 ms)
std dev              4.974 ms   (1.455 ms .. 8.165 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking pearl2/cclique2 20
time                 151.8 μs   (150.4 μs .. 153.6 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 150.8 μs   (150.0 μs .. 152.5 μs)
std dev              3.640 μs   (2.180 μs .. 5.920 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking pearl2/cclique3 20
time                 1.076 μs   (1.064 μs .. 1.088 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.060 μs   (1.053 μs .. 1.070 μs)
std dev              28.21 ns   (21.11 ns .. 40.29 ns)
variance introduced by outliers: 35% (moderately inflated)

benchmarking pearl2/cclique2 50
time                 5.404 ms   (5.342 ms .. 5.469 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 5.406 ms   (5.382 ms .. 5.432 ms)
std dev              78.44 μs   (63.77 μs .. 98.68 μs)

benchmarking pearl2/cclique3 50
time                 2.734 μs   (2.726 μs .. 2.744 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.733 μs   (2.723 μs .. 2.742 μs)
std dev              30.95 ns   (25.35 ns .. 39.40 ns)
-}

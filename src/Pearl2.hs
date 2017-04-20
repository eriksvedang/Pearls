module Pearl2 where

-- P := people at the party
-- C ⊆ P
-- Everybody knows every member of C
-- Members of C only know each other
-- Find C!

nrOfPeople = 4

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

cclique1 :: Maybe [Integer]
cclique1 = let ps = [1..nrOfPeople]
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

cclique2 :: [Integer]
cclique2 = (head . ccliques) [1..nrOfPeople]

-- 3. Fusion!

subseqs' :: [a] -> ([a], [[a]])
subseqs' = foldr step ([], [[]])

step :: a -> ([a], [[a]]) -> ([a], [[a]])
step x (xs, xss) = (x : xs, map (x:) xss ++ xss)

cclique3 :: [Integer]
cclique3 = let ps = [1..nrOfPeople]
               f (ps, css) = head (filter (<§ ps) css)
           in (f . subseqs') ps
  

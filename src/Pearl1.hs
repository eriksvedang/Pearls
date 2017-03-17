module Pearl1 where

import Data.Array
import Data.List (partition)

-- Simplest solution --
type Nat = Int

minfree :: [Nat] -> Nat
minfree xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

ex1 = minfree [5, 10, 0, 4, 3, 2, 1]

-- Array based solution --
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) assocList
               where n = length xs
                     assocList = (zip (filter (<= n) xs) (repeat True))

minfree2 xs = search (checklist xs)

ex2 = minfree2 [5, 10, 0, 4, 3, 2, 1]

-- Note: 'accumArray' can be used to combine several items in the association list that share the same key.
--       The function passed as the first argument is used for this.

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
               where n = maximum xs
                
-- Divide & Conquer solution --

-- (as ++ bs) \\ cs == (as \\ cs) ++ (bs \\ cs)
-- as \\ (bs ++ cs) == (as \\ bs) \\ cs
-- (as \\ bs) \\ cs == (as \\ cs) \\ bs

minfree3 xs = minfrom 0 (length xs, xs)

minfrom :: Nat -> (Int, [Nat]) -> Nat
minfrom a (n, xs) | null xs            = a
                  | length us == b - a = minfrom b (n - m, vs)
                  | otherwise          = minfrom a (m, us)
  where (us, vs) = partition (< b) xs
        b        = a + 1 + n `div` 2
        m        = length us

ex3 = minfree3 [5, 8, 0, 4, 2, 3, 1]

pearl1TestData :: [Nat]
pearl1TestData = [x | x <- [0..10000000], x /= 9999987]

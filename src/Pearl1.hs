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

ex2 = search (checklist [5, 10, 0, 4, 3, 2, 1])

-- QUESTION: The 'accumulator' function (first arg to accumArray) is
--           called for each item that exists in the alist, otherwise
--           just the 'initial value' (False in this case) is used?!

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
               where n = maximum xs
                
-- Divide & Conquer solution --

-- (as ++ bs) \\ cs == (as \\ cs) ++ (bs \\ cs)
-- as \\ (bs ++ cs) == (as \\ bs) \\ cs
-- (as \\ bs) \\ cs == (as \\ cs) \\ bs

-- minfree2 xs = if null ([0 .. b - 1] \\ us)
--               then head ([b..] \\ vs)
--               else head ([0..] \\ us)
--   where (us, vs) = partition (< b) xs

-- minfrom

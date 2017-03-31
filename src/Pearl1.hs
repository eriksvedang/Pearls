module Pearl1 where

import Data.Array
import Data.Array.ST
import Data.List (partition)
import Criterion.Main

-- Simplest solution --
type Nat = Int

minfree1 :: [Nat] -> Nat
minfree1 xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

ex1 = minfree1 [5, 10, 0, 4, 3, 2, 1]

-- Array based solution --
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) assocList
               where n = length xs
                     assocList = (zip (filter (<= n) xs) (repeat True))

checklist' :: [Int] -> Array Int Bool
checklist' xs = runSTArray $ do
  a <- newArray (0, n) False
  sequence [writeArray a x True | x <- xs, x <= n]
  return a
  where n = length xs

minfree2 xs = search (checklist xs)

minfree2' xs = search (checklist' xs)

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
minfrom a (n, xs) | n == 0     = a
                  | m == b - a = minfrom b (n - m, vs)
                  | otherwise  = minfrom a (m, us)
  where (us, vs) = partition (< b) xs
        b        = a + 1 + n `div` 2
        m        = length us

ex3 = minfree3 [5, 8, 0, 4, 2, 3, 1]

pearl1TestData :: Nat -> [Nat]
pearl1TestData n =
  let skip = n - 2
  in  [x | x <- [0..n], x /= skip]

testFunc f n = f (pearl1TestData n)

pearl1 :: IO ()
pearl1 = do
  -- putStrLn ("minfree1: " ++ show (minfree1 pearl1TestData))
  -- putStrLn ("minfree2: " ++ show (minfree2 pearl1TestData))
  -- putStrLn ("minfree2': " ++ show (minfree2' pearl1TestData))
  -- putStrLn ("minfree3: " ++ show (minfree3 pearl1TestData))

  let n = 9999999
  
  defaultMain [
    bgroup "pearl1" [
        --bench "minfree1 (naïve / elegant)" $ whnf (testFunc minfree1) n -- <- SLOW!
        bench "minfree2 (accumArray)" $ whnf (testFunc minfree2) n
        , bench "minfree2' (STArray)" $ whnf (testFunc minfree2') n
        , bench "minfree3 (divide & conquer)" $ whnf (testFunc minfree3) n
        ]
    ]
  

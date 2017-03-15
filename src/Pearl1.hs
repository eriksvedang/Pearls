module Pearl1 where

type Nat = Int

minfree :: [Nat] -> Nat
minfree xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

ex1 = minfree [5, 10, 0, 4, 3, 2, 1]



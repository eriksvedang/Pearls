module Main where

import Pearl1

main :: IO ()
main = do
  a
  b

a = putStrLn ("minfree2: " ++ show (minfree2 pearl1TestData))
b = putStrLn ("minfree3: " ++ show (minfree3 pearl1TestData))


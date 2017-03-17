module Main where

import Pearl1

main :: IO ()
main = do
  putStrLn ("minfree2: " ++ show (minfree2 pearl1TestData))
  putStrLn ("minfree3: " ++ show (minfree3 pearl1TestData))


module Main where

import Day4

main :: IO ()
main = 
    print $ length $ findRightPasswords 0 9999999

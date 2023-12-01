module Main (main) where

import Trebuchet (doTheStuff) 

main :: IO ()
main = 
    do
        s <- readFile "c:\\source\\AoC2023\\inputs\\day1.txt"
        putStr $ show $ doTheStuff $ lines s
        

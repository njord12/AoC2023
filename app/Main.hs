module Main (main) where

import Trebuchet (doTheStuff) 
import CubeConundrum
import CubeConundrum (getAnswer, getAnswer2)

main :: IO ()
main = 
    do
        s <- readFile "c:\\source\\AoC2023\\inputs\\day2.txt"
        putStr $ show $ getAnswer2 $ lines s
        

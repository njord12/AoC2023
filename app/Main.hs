module Main (main) where

import Trebuchet (getAllCalibrationValues) 

main :: IO ()
main = 
    do
        s <- readFile "c:\\source\\AoC2023\\inputs\\day1.txt"
        putStr $ show $ sum $ getAllCalibrationValues $ lines s
        

module Trebuchet (getAllCalibrationValues,processString,replace,spelledDigits, doTheStuff)
where
import Data.Char (isNumber, isAlpha, isDigit)
import Data.List(intercalate)
import Data.List.Split(splitOn)


getNumbersChar :: String -> [Char]
getNumbersChar = filter isNumber

getNumbers :: [Char] -> Int
getNumbers l = read l :: Int

getFirstAndLast :: [a] -> [a]
getFirstAndLast list = head list : [last list]

getCalibrationValue :: String -> Int
getCalibrationValue str = getNumbers $ getFirstAndLast $ getNumbersChar str

getAllCalibrationValues :: [String] -> [Int]
getAllCalibrationValues = map getCalibrationValue 

spelledDigits :: [(String, String)]
spelledDigits = [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]

replace :: String -> String -> String -> String
replace from to = intercalate to . splitOn from

processString :: [(String, String)] -> String -> String
processString [] str = str
processString ((a, b) : xs) str = processString xs (replace a b str) 

doTheStuff :: [String] -> Int
doTheStuff strs = sum $ getAllCalibrationValues $ map (processString spelledDigits) strs
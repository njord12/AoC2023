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
spelledDigits = [("one", "o1e"), ("two", "t2o"), ("three", "t3ree"), ("four", "f4ur"), ("five", "fi5ve"), ("six", "s6x"), ("seven", "sev7n"), ("eight", "eig8t"), ("nine", "n9ne")]

replace :: String -> String -> String -> String
replace from to = intercalate to . splitOn from

processString :: [(String, String)] -> String -> String
processString [] str = str
processString ((a, b) : xs) str = processString xs (replace a b str) 

doTheStuff :: [String] -> Int
doTheStuff strs = sum $ getAllCalibrationValues $ map (processString spelledDigits) strs
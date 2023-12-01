module Trebuchet (getAllCalibrationValues)
where
import Data.Char (isNumber)



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

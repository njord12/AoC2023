module CubeConundrum (getAnswer, getAnswer2)
where

import Data.List (concatMap)
import Data.Char
import Data.List.Split


--getPairs :: [String] -> [(String, Char)]
--getPairs [] = []
--getPairs ("Game ":x:xs) = getPairs xs


getGameNumber :: String -> (Int, String)
getGameNumber ('G':'a':'m':'e':' ': n: ':': rest) = (digitToInt n, rest)
getGameNumber ('G':'a':'m':'e':' ': n1: n2: ':': rest) = (read [n1, n2] ::Int, rest)
getGameNumber ('G':'a':'m':'e':' ': n1: n2:n3: ':': rest) = (read [n1,n2,n3] ::Int, rest)
getGameNumber _ = error "String in wrong format"

getReveals :: String -> [String]
getReveals = splitOn ";"

getPullsForGame :: String -> [(String, Int)]
getPullsForGame str =
    zip colors nums
    where
        l = splitOn "," str
        n' = map (filter isDigit) l
        nums = map (read :: String -> Int) n'
        colors = map (filter isAlpha) l

checkValid :: (String, Int) -> Bool
checkValid (s,n) =
    case s of
        "blue" -> n <= 14
        "green" -> n <= 13
        "red" -> n <= 12
        _ -> False

checkValidPull :: [(String, Int)] -> Bool
checkValidPull = all checkValid

checkValidGame :: [[(String, Int)]] -> Bool
checkValidGame = all ((== True) . checkValidPull)

processGame :: String -> (Int,Bool)
processGame g =
    (n, b)
    where
        (n, pulls) = getGameNumber g
        reveals = getReveals pulls
        x = map getPullsForGame reveals
        b = checkValidGame x

processGamePart2 :: String -> Int
processGamePart2 g =
    power
    where
        (_, pulls) = getGameNumber g
        reveals = getReveals pulls
        x =  concatMap getPullsForGame reveals
        power =  findPower x


aux :: (Int, Bool) -> Int
aux (n, True) = n
aux (_, False) = 0

getAnswer :: [String] -> Int
getAnswer strs = sum $ map (aux . processGame) strs

getAnswer2 :: [String] -> Int
getAnswer2 strs = sum $ map processGamePart2 strs


groupColor :: String -> [(String, Int)] -> [(String, Int)]
groupColor _ [] = []
groupColor s ((s1,n):rest) = if s == s1 then (s,n) : groupColor s rest else groupColor s rest

findPower :: [(String, Int)] -> Int
findPower cls = 
    maxBlue * maxRed * maxGreen
    where
        (_, blueNumbers) = unzip $ groupColor "blue" cls
        (_, redNumbers) = unzip $ groupColor "red" cls
        (_, greenNumbers) = unzip $ groupColor "green" cls
        maxBlue = maximum blueNumbers
        maxRed = maximum redNumbers
        maxGreen = maximum greenNumbers

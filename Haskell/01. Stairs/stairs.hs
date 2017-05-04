module Main where
import System.IO


getDiffBuff :: String -> String -> Int -> Int
getDiffBuff x y i = (if (x !! i) /= (y !! i) then 1 else 0) + if (i /= 0) then getDiffBuff x y (i - 1) else 0

getDiff :: String -> String -> Int
getDiff x y =
    getDiffBuff x y (length x - 1)

isAdj :: String -> String -> Bool
isAdj x y = getDiff x y == 1



main = do
    content <- readFile "dictionary.txt"
    let words = lines content
    let a = words !! 0
    let b = words !! 5
    print(getDiff a b)
    print(isAdj a b)

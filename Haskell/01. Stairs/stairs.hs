module Main where
import System.IO
import Data.HashSet


push :: String -> [String] -> [String]
push element queue = queue ++ [element]


pop :: [String] -> [String]
pop queue = tail queue


peek :: [String] -> String
peek (x:queue) = x


nop :: IO ()
nop = sequence_ []


getDiffBuff :: String -> String -> Int -> Int
getDiffBuff x y i = (if (x !! i) /= (y !! i) then 1 else 0) + if (i /= 0) then getDiffBuff x y (i - 1) else 0


getDiff :: String -> String -> Int
getDiff x y =
    getDiffBuff x y (length x - 1)


isAdjacents :: String -> String -> Bool
isAdjacents x y = getDiff x y == 1


--addAdjacentsBuff :: Int -> [String] -> [String] -> String -> [String]
addAdjacentsBuff currentIndex list queue used word = do
    let currentElement = list !! currentIndex
    if currentIndex /= (length list - 1) && isAdjacents (currentElement) word && not (member currentElement used)
        then (addAdjacentsBuff (currentIndex + 1) list (push (currentElement) queue) (insert currentElement used) word)
        else if (currentIndex == length list - 1)
            then (queue, used)
            else (addAdjacentsBuff (currentIndex + 1) list queue used word)


--addAdjacents :: [String] -> [String] -> String -> [String]
addAdjacents list queue used word =
    addAdjacentsBuff 0 list queue used word


smartPrintBuff currentIndex list = do
    let currentElement = list !! currentIndex
    putStrLn(currentElement)
    if (currentIndex /= length list - 1) then smartPrintBuff (currentIndex + 1) list else nop


smartPrint list = do
    smartPrintBuff 0 list


updateQueue words queue used = do
    let el = peek queue
    addAdjacents words (pop queue) used el


checkUsed used lastWord = do
    print(if (member lastWord used) then "Final!" else "Not final")


loop queue words used lastWord = do
    let q = updateQueue words queue used
    if member lastWord used then used else loop (fst q) words (snd q) lastWord


main = do
    content <- readFile "dictionary.txt"
    let words = lines content
    let used = fromList ["муха"]
    let queue = ["муха"]
    let finalSet = loop queue words used "меха"
    smartPrint $ toList $ finalSet
module Main where
import System.IO
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Dict
import qualified Data.Maybe as Maybe


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
    if currentIndex /= (length list - 1) && isAdjacents (currentElement) word && not (Set.member currentElement used)
        then (addAdjacentsBuff (currentIndex + 1) list (push (currentElement) queue) (Set.insert currentElement used) word)
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


loop queue words used lastWord = do
    if length queue /= 0
        then (if (Set.member lastWord used) || (length used == length words) then used else loop (fst (updateQueue words queue used)) words (snd (updateQueue words queue used)) lastWord)
        else used


main = do
    content <- readFile "dictionary.txt"
    let words = lines content
    let start = "муха"
    let used = Set.fromList [start]
    let queue = [start]
    let a = Dict.insert "3" "2" Dict.empty
    let b = Dict.lookup "3" a
    print(Maybe.fromJust b)
    {-
    let q = updateQueue words queue used
    let q0 = updateQueue words (fst q) (snd q)
    let q1 = updateQueue words (fst q0) (snd q0)
    let q2 = updateQueue words (fst q1) (snd q1)
    let q3 = updateQueue words (fst q2) (snd q2)
    let q4 = updateQueue words (fst q3) (snd q3)
    let q5 = updateQueue words (fst q4) (snd q4)-}
    let finalSet = loop queue words used "кура"
    smartPrint $ Set.toList $ finalSet
    --print(length (snd q1))
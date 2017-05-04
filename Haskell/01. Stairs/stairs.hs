module Main where
import System.IO
import Data.HashSet

data Queue _type = Queue [_type] deriving (Show)

push :: String -> [String] -> [String]
push element queue = queue ++ [element]


pop :: [String] -> (String, [String])
pop queue = (head queue, tail queue)


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


addAdjacentsBuff :: Int -> [String] -> [String] -> String -> [String]
addAdjacentsBuff currentIndex list queue word =
    if currentIndex /= (length list - 1) && isAdjacents (list !! currentIndex) word
        then (addAdjacentsBuff (currentIndex + 1) list (push (list !! currentIndex) queue) word)
        else if (currentIndex == length list - 1)
            then queue
            else (addAdjacentsBuff (currentIndex + 1) list queue word)


addAdjacents :: [String] -> [String] -> String -> [String]
addAdjacents list queue word =
    addAdjacentsBuff 0 list queue word


smartPrintBuff currentIndex list = do
    putStrLn(list !! currentIndex)
    if (currentIndex /= length list - 1) then smartPrintBuff (currentIndex + 1) list else nop


smartPrint list = do
    smartPrintBuff 0 list


main = do
    content <- readFile "dictionary.txt"
    let words = lines content
    let q = addAdjacents words ["муха"] "муха"
    let new_q = snd (pop q)
    smartPrint new_q
    
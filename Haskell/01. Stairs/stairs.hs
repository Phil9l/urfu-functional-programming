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


repl :: String -> Int -> Char -> String
repl s pos c = (take pos s) ++ [c] ++ (drop (pos + 1) s)


getAdjes :: String -> [String]
getAdjes word = concat (map (\pos -> map (\c -> repl word pos c) ['а' .. 'я']) [0 .. (length word) - 1])


findNeighbours :: Set.HashSet String -> String -> [String]
findNeighbours dict word = filter (\x -> Set.member x dict) (getAdjes word)


qmanyAddingBuff :: [String] -> [String] -> Int -> [String]
qmanyAddingBuff queue list index = if (index == (length list)) then queue else qmanyAddingBuff (push (list !! index) queue) list (index + 1)


qmanyAdding :: [String] -> [String] -> [String]
qmanyAdding queue list = qmanyAddingBuff queue list 0


smanyAddingBuff :: Set.HashSet String -> [String] -> Int -> Set.HashSet String
smanyAddingBuff used list index = if (index == (length list)) then used else smanyAddingBuff (Set.insert (list !! index) used) list (index + 1)


smanyAdding :: Set.HashSet String -> [String] -> Set.HashSet String
smanyAdding used list = smanyAddingBuff used list 0


pmanyAddingBuff :: Dict.HashMap String String ->  String -> [String] -> Int -> Dict.HashMap String String
pmanyAddingBuff path word list index = if (index == (length list)) then path else pmanyAddingBuff (Dict.insert (list !! index) word path) word list (index + 1)


pmanyAdding :: Dict.HashMap String String ->  String -> [String] -> Dict.HashMap String String
pmanyAdding path word list = pmanyAddingBuff path word list 0


loop queue used path words finish = do
    if ((length queue) == 0)
        then Dict.empty
        else do
            let buff = peek queue
            let usefulNeirs = filter (\x -> (not(Set.member x used))) (findNeighbours words buff)
            let newQueue = pop $ qmanyAdding queue usefulNeirs
            let newUsed = smanyAdding used usefulNeirs
            let newPath = pmanyAdding path buff usefulNeirs
            if (Set.member finish newUsed) then newPath else (loop newQueue newUsed newPath words finish)


smartPrintBuff words index = do
    if (index == (length words))
        then nop
        else do
            (putStrLn (words !! index))
            smartPrintBuff words (index + 1)

smartPrint words = do smartPrintBuff words 0


recoverPathBuff path start finish current finalPath = if (current == start) then finalPath else (recoverPathBuff path start finish (Maybe.fromJust (Dict.lookup current path)) (finalPath ++ [current]))
recoverPath path start finish = recoverPathBuff path start finish finish []
main = do
    content <- readFile "dict.txt"
    start <- getLine
    finish <- getLine
    putStrLn("======================")
    let words = Set.fromList (filter (\x -> (length x) == (length start)) (lines content))
    let path = Dict.insert start "xxxx" Dict.empty
    let used = Set.insert start Set.empty
    let queue = [start]
    let newPath = loop queue used path words finish
    if (newPath == Dict.empty)
        then putStrLn("Пути не существует")
        else smartPrint(reverse ((recoverPath newPath start finish) ++ [start]))

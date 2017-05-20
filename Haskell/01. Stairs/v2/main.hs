import Prelude hiding (lookup)
import qualified Data.Map as Map
import Data.List

-- Проверяем, что слова отличаются на одну букву.
oneDiff :: String -> String -> Bool
oneDiff w1 w2 = if getDistance 0 w1 w2 == 1 then True else False
    where
        getDistance n [] [] = n
        getDistance n (x1:xs1) (x2:xs2) = if x1 /= x2
            then getDistance (n+1) xs1 xs2
            else getDistance n xs1 xs2

-- Ищем соседей для поиска в ширину(все слова из словаря отличающиеся на одну букву от заданного).
getNeighbors :: String -> [String] -> [String]
getNeighbors word dict = filter (\w -> oneDiff w word) dict

-- Добавляем слова в список посещенных слов.
mark :: [String] -> [String] -> [String]
mark visited [] = visited
mark visited toVisited = mark ((head toVisited) : visited) (tail toVisited)

-- Ищем следующие подходящие слова(те которые отличаются на одну букву и те которые мы еще не смотрели).
getNextNodes :: [String] -> String -> [String] -> [String]
getNextNodes graph node visited = (filter (\ x -> not (elem x visited)) (getNeighbors node graph))

-- Для восстановления пути храним словарь, где по каждому слову записано слово, из которого мы в него пришли.
setParent :: Map.Map String String-> String -> [String] -> Map.Map String String
setParent parents node [] = parents
setParent parents node children = setParent (Map.insert (head children) node (parents)) node (tail children)


bfs1 :: String -> [String] -> [String] -> [String] -> Map.Map String String -> Map.Map String String
bfs1 finish graph [] visited parents = parents
bfs1 finish graph queue visited parents = if (head queue) == finish then parents
                                          else bfs1 finish graph ((tail queue) ++ nextNodes) (mark visited nextNodes) (setParent parents (head queue) nextNodes)
                                          where nextNodes = getNextNodes graph (head queue) visited


getPath :: String -> String -> Map.Map String String -> [String] -> [String]
getPath start node parents path = if node == start then path
                                  else getPath start (parents Map.! node) parents ((parents Map.! node):path)

bfs :: String -> String -> [String] -> [String]
bfs start finish words = if (Map.notMember finish parents) then ["Нет нужного пути"]
                                                           else getPath start finish parents [finish]
                                                           where parents = bfs1 finish words [start] [start] Map.empty; 
                                                                 parentkeys = Map.keysSet(parents)
main = do
    s <- readFile "runouns1.txt"
    let words = lines s
    putStrLn "Введите первое слово:"
    w1 <- getLine
    putStrLn "Введите второе слово:"
    w2 <- getLine
    let filter_words = filter(\x -> length x == length w1) words
    let path = bfs w1 w2 filter_words
    putStrLn ""
    putStr(unlines path)

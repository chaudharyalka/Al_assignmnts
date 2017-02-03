import Data.Char
import Data.List

data Tree a =  Node a [Tree a]  deriving (Eq, Show)

t1 = Node 5 [ t2 , t3 , t4]
t2 = Node 6 [Node 8 [Node 7 []]]
t3 = Node 54 [Node 9 []]
t4 = Node 34 []

height (Node a []) = 1
height (Node a ls) = 1 + (maximum . map height $ ls)

sum_tree (Node a []) = a
sum_tree (Node a ls) = a + (sum . map sum_tree $ ls)

-- by using concat and map:

dfs (Node a []) = [a]
dfs (Node a ls) = [a] ++ (concat . map dfs $ ls)

bfs ([]) = []
bfs (ls) =  concat (map children ls) --  (map root ls) ++ bfs (concat (map children ls))

root (Node a ls) = a
children (Node a ls) = ls

-- list comprehension:

dfs2 (Node a []) = [a]
dfs2 (Node a ls) = [a] ++ [ y |  x <- map dfs $ ls , y <- x] 

bfs2 ([]) = []
bfs2 (ls) = (map root ls ) ++ bfs2 [y | x <- map children $ ls , y <- (x)]






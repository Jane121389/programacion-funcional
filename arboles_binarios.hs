data BTree a = Void | Node a (BTree a) (BTree a) deriving (Show,Eq)

leaf x = Node x Void Void


et1 = Node 3 (Node 2 (leaf 1) (leaf 2)) (Node 7 (leaf 3) (Node 8 (leaf 4) (leaf 5)))


size (Void) = 0

size (Node x t1 t2) = 1 + size t1 + size t2

height (Void) = 0

height (Node x t1 t2) = 1 + max (height t1) (height t2) 


elemT x Void = False

elemT x (Node y l r) = x == y || elemT x l || elemT x r


--Recorridos
-- 3 versiones de flat

preorden Void = []

preorden (Node x l r) = x:preorden l ++ preorden r


postorden Void = []

postorden (Node x l r) = postorden l ++ postorden r ++ [x]


inorden Void = []

inorden (Node x l r) = postorden l ++ [x] ++ postorden r 




--Recorrido por amplitud (bfs)



level Void _ = []

level (Node x l r) 0 = [x]

level (Node x l r) n = level l (n-1) ++ level r (n-1)

levels t = [level t k | k <- [0..height t]]

bfs = concat . levels 


-- Unión de árboles


merge Void t = t

merge t Void = t

merge t1@(Node x l r) t2 
  | size t1 >= size t2 = Node x (merge s t2) b
  | otherwise = merge t2 t1
 where (s,b) = if size l <= size r then
                                    (l,r) 
                                 else
                                    (r,l)


ins x Void = leaf x

ins x t = merge (leaf x) t

    
--mkBTree = foldr ins Void 

mkBTree [] = Void

mkBTree (x:xs) = ins x (mkBTree xs)


--eliminar un elemento

--caso de listas

del x [] = []

del x (y:ys) = if x == y then ys else (y:del x ys)

delAll x [] = []

delAll x (y:ys) = if x == y then delAll x ys else (y:delAll x ys)


-- eliminación de la primera presencia de un elemento

delT x Void = Void

delT x t@(Node y l r)
 | x == y = merge l r
 | elemT x l = Node y (delT x l) r
 | elemT x r = Node y l (delT x r)
 | otherwise = t              


-- eliminación de todas las presencias de un elemento

delAllT x Void = Void

delAllT x (Node y l r) = if x == y then merge (delAllT x l) (delAllT x r)
                                   else Node y (delAllT x l) (delAllT x r)      


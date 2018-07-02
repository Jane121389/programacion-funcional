
{--1. Implemente una función numbf :: Tree a ->Tree Int tal que numbf t devuelve un árbol con la misma forma que t pero donde los nodos est ́an numerados desde 1 de acuerdo al recorrido por amplitud (bfs) en t-}

data BTree a = Void | Node a (BTree a) (BTree a) deriving (Show,Eq)
data Direccion = Izq | Der deriving (Show, Eq)

arbolin = Node 1 (Node 2 (Void)(Node 1 (Node 1 Void Void) (Node 3 Void Void))) (Node 3 (Node 7 Void (Node 2 Void Void))(Node 4 Void (Node 2 Void Void)))

cuantosEnNivel :: BTree a -> Int -> Int
cuantosEnNivel (Node a x y) 1 = 1
cuantosEnNivel Void n = 0
cuantosEnNivel (Node a x y) nivel =  cuantosEnNivel x (nivel-1) + cuantosEnNivel y (nivel -1)

cuantosAMiIzquierda :: BTree a -> [Direccion] -> Int
cuantosAMiIzquierda t [] = 0
cuantosAMiIzquierda t@(Node a x y) c@(d:ds) = if d == Der
then cuantosEnNivel x (length c) + cuantosAMiIzquierda y ds
else cuantosAMiIzquierda x ds

numElem:: BTree a -> Int -> Int
numElem Void n = 0
numElem (Node a x y) 1 = 1
numElem (Node a x y) n = numElem x (n-1) + numElem y (n-1) + 1

indice:: BTree a -> [Direccion] -> Int
indice x [] = 1
indice x ds = numElem x (length ds) + cuantosAMiIzquierda x ds+1


retiqueta2 :: BTree a -> BTree a -> [Direccion] -> BTree Int
retiqueta2 Void raiz camino = Void
retiqueta2 (Node a x y) raiz camino = (Node
         (indice raiz camino)
         (retiqueta2 x raiz (camino ++ [Izq]))
         (retiqueta2 y raiz (camino ++ [Der])))
         
numbf :: BTree a -> BTree Int
numbf (Node a x y) = retiqueta2 (Node a x y) (Node a x y) []


{--2. Considere el siguiente lenguaje simple de expresiones aritméticas:
e ::= n | e + e | e − e | e ∗ e | e/e | try e e
Este lenguaje puede implementarse mediante el siguiente tipo:

data Exp = Val Int |
Add Int Int |
Sub Int Int |
Prod Int Int |
Div Int Int |
Try Int Int

1a) Defina un evaluador eval: Exp ->Int que devuelve el valor entero de una expresión.
Por el momento las expresiones de la forma Try e1 e2 deben devolver el valor de e1.--}


data Exp = Val Int |Add Exp Exp|Sub Exp Exp |Prod Exp Exp|Div Exp Exp |Try Exp Exp

eval::Exp->Int

eval (Val x)=x

eval (Add x y)=(eval x) + (eval y)

eval (Sub x y )=(eval x) - (eval y)

eval (Prod x y)= (eval x) * (eval y)

eval (Div x y)= div (eval x) (eval y)

eval (Try x y)= eval x

{--b) Supongamos ahora que queremos usar el tipo Exp para modelar cómputos de enteros positivos con un valor máximo fijo, denotado maxInt y donde sólo permitimos divisiones exactas (es decir, con residuo cero). En este caso pueden surgir diversas excepciones definidas mediante el tipo enumerado Exception, cuyos elementos son:
NegNExc: se produjo un número negativo
MaxNExc: se produjo un número mayor al máximo fijo
DivZExc: se intentó dividir entre cero.
RemExc: se intentó una división no exacta (con residuo distinto de cero)
Defina un evaluador con manejo de excepciones, evalHdl: Exp ->Excep Int de manera que
si surge alguna de las anteriores excepciones, se propague y en el caso de una expresión Try e1 e2 si e1 lanza una excepción, el valor devuelto será el de e2. Aqu ́ı Excep a es el siguiente
tipo:
data Excep a = Ok a | Throw Exception

Por ejemplo, si maxInt = 1000, entonces--}

data Excep a = Ok a | Throw Exception deriving (Show)

data Exception= NegNExc|MaxNExc|DivZExc|RemExc deriving (Show)

maxInt=1000;

evalH::Exp->Excep Int

isThrow::Excep a->Bool

isThrow (Ok a)=False

isThrow (Throw x) =True

unWrap ::Excep Int-> Int

unWrap (Ok x) = x

evalua :: Int->Excep Int

evalua x | x>maxInt = Throw MaxNExc
         | x<0 =  Throw NegNExc
         | otherwise = Ok x

evalH (Val x) = Ok x

evalH (Add x y) | isThrow rx = rx
                | isThrow ry = ry
                | otherwise = evalua (unWrap  rx + unWrap ry)
                where ry = evalH y
                      rx = evalH x
evalH (Sub x y) | isThrow rx = rx
                | isThrow ry = ry
                | otherwise = evalua (unWrap  rx + unWrap ry)
                where ry = evalH y
                      rx = evalH x
evalH (Prod x y)| isThrow rx = rx
                | isThrow ry = ry
                | otherwise = evalua (unWrap  rx + unWrap ry)
                where ry = evalH y
                      rx = evalH x
evalH (Div x y) | isThrow rx = rx
                | isThrow ry = ry
                | unWrap ry == 0 = Throw DivZExc
                | mod (unWrap rx) (unWrap ry) /=0 = Throw RemExc
                | otherwise = evalua (div (unWrap  rx) (unWrap ry))
                where ry = evalH y
                      rx = evalH x
evalH (Try x y) | isThrow rx = ry
                | otherwise = rx
                where ry = evalH y
                      rx = evalH x

evalHdl:: Exp ->Excep Int

evalHdl (Val x)=evalua x;

evalHdl x =evalH x

exp1 =Val (-1)
exp2 =Val 200
exp3= Div (Val 8) (Val 10)
exp4= Div (Val 8) (Val 0)
exp5= Add (Val 10) (Val (-1))
exp6= Try exp4  exp5
exp7= Add (Val 3) exp4

{--3. Defina las siguientes listas infinitas cuyos elementos son de nuevo listas :
-La lista [Pot(2), Pot(3), . . . , Pot(n), . . .] donde Pot(n) = [1, n, n^2 , n^3 , . . .]--}

pot::Integer->[Integer]

pot n=[n^s|s<-[1..]]

listainf1=[pot n|n<-[2..]]

{---La lista infinita cuyos elementos son
[ ]
[(1, 1)]
[(2, 1), (1, 2)]
[(3, 1), (2, 2), (1, 3)]
[(4, 1), (3, 2), (2, 3), (1, 4)] . . .--}

pares n=[(x,y)|x<-[1..n],y<-[1..n],x+y==n]

listainf2=[reverse (pares n)|n<-[1..]]

{--La lista infinita cuyos elementos son listas de coeficientes del triangulo de Pascal, es decir la lista
[ [1], [1, 1], [1, 2, 1], [1, 3, 3, 1], [1, 4, 6, 4, 1], . . .]

--}
coef::Integer->Integer->Integer

coef n 0 = 1

coef 0 k = 0

coef n k = div (n *coef (n-1)(k-1)) k

listacoef n=[coef n x|x<-[0..n]]

listainf3=[listacoef n|n<-[0..]]

{-- 4. Sean k > 0, a 0 , . . . , a k−1 constantes dadas y (c n ) n≥k una sucesión dada. El ejercicio consiste
en programar una función gensuc que genere la sucesi ́on (x i ) i∈N definida como sigue:
x 0 = a 0 , . . . , x k−1 = a k−1 , x n = f (c n , x n−1 , . . . , x n−k )
donde n ≥ k y f es una función cualquiera.
Utilizando la definición general programar las siguientes sucesiones:
x 0 = 1, x 1 = 2, x n+2 = (n + 1)x n+1 + x n
x 0 = 1, x 1 = 1, x n+2 = nx n+1 + 3x n
--}

gensuc:: [Integer]->[Integer]->([Integer]->Integer)->[Integer]

suc x [] f n=[]

suc x (y:ys) f n= cad:suc (x++[cad]) ys f n where cad=f (y:reverse (take n (reverse x)))

gensuc x y f= x ++ suc x y f (length x)

func1 x = (x!!0) * (x!!2) + (x !! 1)

c=[1,2]

s=[1..]

sucesion1= gensuc c s func1

func2 x = (x!!0) * (x!!2) + 3*(x !! 1)

c2=[1,1]

s2=[0..]

sucesion2= gensuc c2 s2 func2

{-- 5. Sintetice una definición recursiva de la siguiente función incl que decide si una lista está incluida en otra (es decir incl xs ys decide si todos los elementos de xs están en ys)
incl xs ys = foldl (&&) True (map (‘elem‘ ys) xs)
Sugerencia: la ley de fusión de foldl es de gran utilidad.--}


{--Empezando la sintesis--

mapRecursivo :: (a->b) -> [a] -> [b]
mapRecursivo f [] = []
mapRecursivo f (x:xs) = (f x):(mapRecursivo f xs)

incl :: (Eq a) => [a] -> [a] -> Bool
incl xs ys = foldl (&&) True (map (`elem` ys) xs)

estaEnLaLista lista elemento = elem elemento lista

listaDetectados lista1 lista2 = mapRecursivo (estaEnLaLista lista2) lista1
listaDetectados1 [] lista2 = []
listaDetectados1 (x:xs) lista2 = (elem x lista2):(listaDetectados1 xs lista2)

elem1 x [] = False
elem1 x (y:ys) = if x == y then True else elem1 x ys

incl1 :: (Eq a) => [a] -> [a] -> Bool
incl1 [] ys = True
incl1 (x:xs) ys = (elem x ys) && incl1 xs ys--}

incl :: (Eq a) => [a] -> [a] -> Bool
incl [] ys = True
incl (x:xs) z@(y:ys) = (elemR x z) && incl xs (y:ys)
                    where elemR x [] = False
                          elemR x (y:ys) = if x == y then True else elemR x ys



{--1. Considere la siguiente especificación de una función inv:
inv :: (a -> Bool) -> [(a,b)] -> [(b,a)]
inv p xs = map swap (filter (p.fst) xs) where swap (x,y) = (y,x)
a) Dé una especificación de inv en español (en un comentario dentro del script).
b) Obtenga una implementación recursiva invr de inv, utilizando el proceso de síntesis de programas a partir de la especificación. El proceso de razonamiento ecuacional para la síntesis del programa debe mostrarse en el script como un comentario, o bien puede usarse un script lhs. En cualquier caso deben definirse en el código las 2 versiones inv e invr--}


inv :: (a -> Bool) -> [(a,b)] -> [(b,a)]

inv p xs = map swap (filter (p.fst) xs) where swap (x,y) = (y,x)

{-- Especificación: invr p xs=Intercambia los elementos de todas las tuplas que cumplen p (fst x) =true--}


{--  Caso base donde xs=[]  entonces  invr p []=[], debido a que [] no tiene ningún elemento entonces el resultado es []

caso inductivo: se tienen dos casos 

i) p fst x=true entonces => swap x: todos los elementos de xs que cumplen p (fst x) =true, que es lo mismo que swap x:invr p xs

ii) p fst x=false entonces => todos los elementos de xs que cumplen p (fst x) =true, que es lo mismo que invr p xs

por lo tanto --}

invr p []=[]

invr p (x:xs)= if (p (fst x)) then (swap x):invr p xs else invr p xs where swap (x,y) = (y,x)

{--2. Considere la siguiente especificación de una función lookup:
lookup :: [(a,b)] -> a -> b
lookup ps k = head (map snd (filter ((==k).fst) ps))
Haga lo mismo que en ejercicio anterior definiendo una función lookupr. Observe que la función lookup no está definida cuando ps es la lista vacía.--}

--lookup :: [(a,b)] -> a -> b

lookup ps k = head (map snd (filter ((==k).fst) ps))

{--Especificación:lookup ps k=toma el segundo elemento de la primera tupla de ps, donde el primer elemento de la tupla es k --}

{--  Caso base donde s=[]  entonces  lookupr p []="error" ya que no existe ningún elemento

caso inductivo: lookup (p:ps) k=toma el segundo elemento de la primera tupla de (p:ps), donde el primer elemento de la tupla es k, como el primer elemento de p:ps es p entonces se tienen dos casos 

i) fst p =k entonces => snd p

ii) fst p /=k entonces => toma el segundo elemento de la primera tupla de ps, donde el primer elemento de la tupla es k, que es lo mismo que lookup ps k

por lo tanto --}

lookupr [] k=error "no existe el elemento"

lookupr (p:ps) k=if (fst p)==k then (snd p) else lookupr ps k


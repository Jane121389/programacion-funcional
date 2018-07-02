{--Considere las siguientes funciones

suc i = i+1
pre i = i-1

La función  suma en enteros add puede definirse utilizando únicamente suc y pre 

add i 0=i

add i j=suc (add i (pre j))

Realice lo siguiente:

1.-Deefinca la función producto de enteros mult de manera análoga, usando unicamente add y pre--}

suc i = i+1

pre i = i-1

add i 0 = i

add i j=suc (add i (pre j))

mult 0 y = 0
 
mult 1 y= y

mult x y = add (mult (pre x) y) y


{--2.-Defina la función exponenciación de enteros exp de manera análoga, usando mult y pre--}

expo x 0 = 1
 
expo x 1= x

expo x y = mult (expo x (pre y)) x

{--3.- ¿Que función sigue?--}

superexpo x 0 = 1
 
superexpo x 1= x

superexpo x y = expo (superexpo x (pre y)) x

{--4.- La función fold para enteros foldi se define como sigue:

        foldi q 0=q
        
        foldi f q j =f (foldi f q (pre j))

a) Dé el tipo de foldi y la especificación análoga a la de foldr--}

foldi::(Int->a->a)->a->Int->a

foldi f q 0=q
        
foldi f q j=f j (foldi f q (pre j ))

{--b)Defina a las funciones add, mult, exp, como instancias de foldi--}

addfoldi i j= foldi (\x y ->suc y) i j

multfoldi i j= foldi (\x y->add y i) 0 j

expofoldi i j= foldi (\x y->mult y i) 1 j

{--c) defina las funciones factorial y fibonacci como instancias de foldi--}

--factofoldi n = foldi (\x y ->mult y x) 1 n

factofoldi n = foldi mult 1 n

fibofoldi n = head (foldi (\x (y:z:zs)->(y+z):y:z:zs) [1,0] (n-1))

--fibofoldi n = foldi mult 1 n


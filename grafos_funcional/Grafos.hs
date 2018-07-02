import Data.Array
type Graph n w= Array n [(n,w)]

grafo1= mkGraph False (1,5) [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]

mkGraph:: (Ix n, Num w)=>Bool->(n,n)->[(n,n,w)]->(Graph n w)

mkGraph dir bounds lista =
	accumArray (\xs x-> x:xs) [] bounds
			   ([(x1,(x2,w))|(x1,x2,w)<-lista]++
			   		if dir then []
			   		else [(x2,(x1,w))|(x1,x2,w)<-lista, x1/=x2])
			   	
adjacent::(Ix n, Num w)=>(Graph n w)->n->[n]
			   		
adjacent g v = map fst (g!v)

node::(Ix n, Num w)=>(Graph n w)->[n]

node g = indices g

edgeIn::(Ix n, Num w)=>(Graph n w)->(n,n)->Bool

edgeIn g (x,y)= elem y (adjacent g x)

weight::(Ix n, Num w)=>n->n->(Graph n w)->w

weight x y g= head [c|(a,c)<-g!x,(a==y)]

edgesD::(Ix n, Num w)=>(Graph n w)->[(n,n,w)]

edgesD g=[(v1,v2,w)|v1 <- node g, (v2,w)<-g!v1]

edgesU::(Ix n, Num w)=>(Graph n w)->[(n,n,w)]

edgesU g=[(v1,v2,w)|v1 <- node g, (v2,w)<-g!v1, v1<v2]

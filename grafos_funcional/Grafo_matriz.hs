import Data.Array
type Graph n w= Array (n,n) (Maybe w)

grafo1= mkGraph False (1,5) [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]

mkGraph:: (Ix n, Num w)=>Bool->(n,n)->[(n,n,w)]->(Graph n w)

mkGraph dir bnds@(l,u) lista =
	emptyArray // ([((x1,x2),Just w)|(x1,x2,w)<-lista]++
			  		if dir then []
			   		else [((x1,x2),Just w)|(x1,x2,w)<-lista, x1/=x2])
	where emptyArray =array ((l,l),(u,u)) [((x1,x2), Nothing)|
											x1<- range bnds,
											x2<- range bnds]
			   	
adjacent::(Ix n, Num w, Eq w)=>(Graph n w)->n->[n]
			   		
adjacent g v1 = [v2|v2<-node g,(g!(v1,v2))/=Nothing]

node::(Ix n, Num w)=>(Graph n w)->[n]

node g = range (l,u) where ((l,_),(u,_))= bounds g

edgeIn::(Ix n, Num w, Eq w)=>(Graph n w)->(n,n)->Bool

edgeIn g (x,y)= (g!(x,y))/=Nothing

weight::(Ix n, Num w)=>n->n->(Graph n w)->w

weight x y g= w where (Just w) =g! (x,y)

edgesD::(Ix n, Num w, Eq w)=>(Graph n w)->[(n,n,w)]

edgesD g=[(v1,v2,unwrap(g!(v1,v2)))|v1 <- node g, v2<-node g, edgeIn g (v1,v2)] where unwrap (Just w) = w

edgesU::(Ix n, Num w, Eq w)=> (Graph n w)->[(n,n,w)]

edgesU g= let (_,(u,_)) = bounds g in [(v1,v2,unwrap(g!(v1,v2)))|v1 <- node g, v2<-range (v1,u), edgeIn g (v1,v2)] where unwrap (Just w) = w



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

grafo2= mkGraph True (1,6) [(1,2,1),(1,3,1),(1,4,1),(3,6,1),(5,4,1),(6,2,1),(6,5,1)] 

depthFirstSearch:: (Ix n, Num w)=> n->(Graph n w)->[n]

depthFirstSearch start g =reverse (dfs [start] [])
	where
		dfs[] vis = vis
		dfs (c:cs) vis
			| elem c vis =dfs cs vis
			| otherwise = dfs ((adjacent g c)++cs) (c:vis)
			

breadthFirstSearch:: (Ix n, Num w)=> n->(Graph n w)->[n]

breadthFirstSearch start g =reverse (bfs [start] [])

	where
		bfs[] vis = vis
		bfs (c:cs) vis
			| elem c vis = bfs cs vis
			| otherwise = bfs (cs++(adjacent g c)) (c:vis)			

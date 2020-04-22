(* structure G = FuncGraph(struct *)
(* 			 type ord_key = int *)
(* 			 val compare = Int.compare *)
(* 			 end) *)
(* structure GL = FuncGraph(struct *)
(* 			  type ord_key = temp *)
(* 			  val compare = Int.compare *)
(* 			  end) *)


structure Liveness : LIVENESS =
struct

type data = MakeGraph.data

datatype igraph =
	 IGRAPH of {graph: unit GL.graph,
		    tnode: Temp.temp -> unit GL.node,
		    gtemp: unit GL.node -> Temp.temp,
		    moves: (unit GL.node * unit GL.node) list}

structure Map = SplayMapFn(struct
			    type ord_key = int
			    val compare = Int.compare
			    end)
(* structure Set = SplaySetFn(struct *)
(* 			    type ord_key = Temp.temp *)
(* 			    val compare = Temp.compare *)
(* 			    end) *)
structure Set = Temp.Set

type livenessData = {liveIn: Set.set, liveOut: Set.set}

fun areMapsEqual (map1, map2, graph) =
    let val (ans, _) = foldl ( fn (node, (state, index)) =>
						    let val {liveIn = liveIn1, liveOut = liveOut1} = valOf(Map.find(map1, index))
							val {liveIn = liveIn2, liveOut = liveOut2} = valOf(Map.find(map2, index))
						    in
							(state
							 andalso Set.equal(liveIn1, liveIn2)
							 andalso Set.equal(liveOut1, liveOut2), index + 1)
						    end) (true, 0) (G.nodes graph)
    in
	ans
    end
			
fun initLivenessMap graph = foldl (fn (node, map) => Map.insert(map, G.getNodeID(node),
								{liveIn = Set.empty, liveOut = Set.empty})) Map.empty (G.nodes(graph))
		       
fun livenessIter (lMap, graph) =
    let val oldMap = lMap
	val nodeList = List.rev(G.nodes(graph))

	fun dataflowCompute (node, curLiveMap) =
	    let val {uses, defs, isMove} : data = G.nodeInfo(node)
		val defsSet = Set.addList(Set.empty, defs)
		val usesSet = Set.addList(Set.empty, uses)
		val updatedOut = G.foldSuccs
				     (fn (succ, curSet)
					 => Set.union(case Map.find(curLiveMap, succ) of
							  NONE => (print("Successor " ^ Int.toString(succ) ^ " not in liveness map\n"); Set.empty)
							| SOME ({liveIn, liveOut}) => liveIn
						     , curSet))
				     Set.empty node
				     
		val updatedIn = Set.union(usesSet, Set.difference(updatedOut, defsSet))
	    in
		Map.insert(curLiveMap, G.getNodeID(node), {liveIn = updatedIn, liveOut = updatedOut})
	    end
    in
	let val newMap = foldl dataflowCompute lMap nodeList
	in
	    if (areMapsEqual(oldMap, newMap, graph))
	    then newMap
	    else livenessIter(newMap, graph)
	end
    end

fun interferenceHelper (graph, liveMap) =
    let val empty : (unit GL.graph) = GL.empty
	fun createNodes (curNode, curGraph) =
	    let val {uses, defs, isMove} : data = G.nodeInfo(curNode)
		fun addTemp (t, g) = GL.addNode(g, t, ())
	    in
		foldl addTemp curGraph (uses @ defs)
	    end

	fun createEdges (id, {liveIn, liveOut}, curGraph (* : GL.graph*)) =
	    let val {uses, defs, isMove} : data = G.nodeInfo(G.getNode(graph, id))
							
		fun addEdge (curTemp, g (*: GL.graph*)) = (case defs of [d] => if isMove
								then (case uses of [u] => (case Temp.compare(curTemp, u) of EQUAL => g
															  | _ => GL.doubleEdge(g, curTemp, d)) 
										 | _ => (print("Move Instruction has more than one Use??");
											 GL.doubleEdge(g, curTemp, d))) 
								else GL.doubleEdge(g, curTemp, d)
						       | list => (foldl (fn (def, gr) => GL.doubleEdge(gr, curTemp, def)) g list))
	    in
		Set.foldl addEdge curGraph liveOut
	    end
	val nodegraph = foldl createNodes empty (G.nodes(graph))
    in
	Map.foldli createEdges nodegraph liveMap
    end

fun moveList (interGraph, cfg) =
    let fun updateList (curNode, list) =
	    case G.nodeInfo curNode of {uses = [use], defs = [def], isMove = true} => list @ [(GL.getNode(interGraph, use), GL.getNode(interGraph, def))]
										   | _ => list
    in
	foldl updateList [] (G.nodes(cfg))
    end
	
fun interferenceGraph fg =
    let val livenessMap = livenessIter(initLivenessMap(fg), fg)
	val interferenceG = interferenceHelper(fg, livenessMap) (* *)
	val movesList = moveList(interferenceG, fg)
	val igraph = IGRAPH{graph = interferenceG,
			    tnode = (fn temp => GL.getNode(interferenceG, temp)),
			    gtemp = GL.getNodeID,
			    moves = movesList}
	fun getLiveOut node = case Map.find(livenessMap, G.getNodeID node) of SOME({liveIn, liveOut}) => Set.listItems(liveOut)
									    | _ => (print("node not in livenessMap"); []) 
    in
	(igraph, getLiveOut)
    end


end

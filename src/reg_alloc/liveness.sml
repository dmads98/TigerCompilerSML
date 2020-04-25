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
	 IGRAPH of {graph: unit LG.graph,
		    tnode: Temp.temp -> unit LG.node,
		    gtemp: unit LG.node -> Temp.temp,
		    moves: (unit LG.node * unit LG.node) list}

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

fun printNode(id, node) = (Int.toString(id))

fun show(out, g as IGRAPH{graph, tnode, gtemp, moves})) = LG.printGraph printNode graph 

fun areMapsEqual (map1, map2, graph) =
    let val (ans, _) = foldl ( fn (node, (state, index)) =>
						    let val {liveIn = liveIn1, liveOut = liveOut1} = valOf(Map.find(map1, index))
							val {liveIn = liveIn2, liveOut = liveOut2} = valOf(Map.find(map2, index))
						    in
							(state
							 andalso Set.equal(liveIn1, liveIn2)
							 andalso Set.equal(liveOut1, liveOut2), index + 1)
						    end) (true, 0) (FG.nodes graph)
    in
	ans
    end
			
fun initLivenessMap graph = foldl (fn (node, map) => Map.insert(map, FG.getNodeID(node),
								{liveIn = Set.empty, liveOut = Set.empty})) Map.empty (FG.nodes(graph))
		       
fun livenessIter (lMap, graph) =
    let val oldMap = lMap
	val nodeList = List.rev(FG.nodes(graph))

	fun dataflowCompute (node, curLiveMap) =
	    let val {uses, defs, isMove} : data = FG.nodeInfo(node)
		val defsSet = Set.addList(Set.empty, defs)
		val usesSet = Set.addList(Set.empty, uses)
		val updatedOut = FG.foldSuccs
				     (fn (succ, curSet)
					 => Set.union(case Map.find(curLiveMap, succ) of
							  NONE => (print("Successor " ^ Int.toString(succ) ^ " not in liveness map\n"); Set.empty)
							| SOME ({liveIn, liveOut}) => liveIn
						     , curSet))
				     Set.empty node
				     
		val updatedIn = Set.union(usesSet, Set.difference(updatedOut, defsSet))
	    in
		Map.insert(curLiveMap, FG.getNodeID(node), {liveIn = updatedIn, liveOut = updatedOut})
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
    let val empty : (unit LG.graph) = LG.empty
	fun createNodes (curNode, curGraph) =
	    let val {uses, defs, isMove} : data = FG.nodeInfo(curNode)
		fun addTemp (t, g) = LG.addNode(g, t, ())
	    in
		foldl addTemp curGraph (uses @ defs)
	    end

	fun createEdges (id, {liveIn, liveOut}, curGraph (* : GL.graph*)) =
	    let val {uses, defs, isMove} : data = FG.nodeInfo(FG.getNode(graph, id))
							
		fun addEdge (curTemp, g (*: GL.graph*)) =
		    (case defs of [d] =>
				  if isMove
				  then (case Temp.compare(curTemp, List.hd(uses)) of EQUAL => g
										   | _ => LG.doubleEdge(g, curTemp, d)) 
											 
				  else LG.doubleEdge(g, curTemp, d)
				| list => (foldl (fn (def, gr) => LG.doubleEdge(gr, curTemp, def)) g list))
	    in
		Set.foldl addEdge curGraph liveOut
	    end
	val nodegraph = foldl createNodes empty (FG.nodes(graph))
    in
	Map.foldli createEdges nodegraph liveMap
    end

fun moveList (interGraph, cfg) =
    let fun updateList (curNode, list) =
	    let val {defs = defs, uses = uses, isMove = isMove} = FG.nodeInfo(curNode)
		val defNode = LG.getNode(interGraph, List.hd(defs))
		val useNode = LG.getNode(interGraph, List.hd(uses))
	    in
		if isMove = true andalso not (LG.isAdjacent(defNode, useNode))
		then list @ [(useNode, defNode)]
		else list
	    end
    in
	foldl updateList [] (FG.nodes(cfg))
    end
	
fun interferenceGraph (fg : MakeGraph.data FG.graph) =
    let val livenessMap = livenessIter(initLivenessMap(fg), fg)
	val interferenceG = interferenceHelper(fg, livenessMap) (* *)
	val movesList = moveList(interferenceG, fg)
	val igraph = IGRAPH{graph = interferenceG,
			    tnode = (fn temp => LG.getNode(interferenceG, temp)),
			    gtemp = LG.getNodeID,
			    moves = movesList}
(*
	fun getLiveOut node = case Map.find(livenessMap, G.getNodeID node) of SOME({liveIn, liveOut}) => Set.listItems(liveOut)
									    | _ => (print("node not in livenessMap"); []) *)
    in
	igraph
    end


end

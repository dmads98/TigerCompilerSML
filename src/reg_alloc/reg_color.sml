structure Color : COLOR =
struct

structure Frame : FRAME = MipsFrame
type allocation = Frame.register Temp.Table.table

structure TempSet = Temp.Set
structure TempMap = Temp.Map

fun color {interference = Liveness.igraph{graph, tnode, moves}, initial = init, spillCost = spillCost, registers = regs} =
    let
	(* isSome, valOf may be useful *)

	fun isPrecolored var =
	    case Temp.Map.find (initial, var) of
		SOME x => true
	      | NONE => false

	(* Returns new graph and node that can be colored *)
	(* (graph * nodelist) -> (graph * nid option) *)
	fun simplify (graph, []) = (graph, NONE)
	  | simplify (graph, hdNode::tlNodes) = 
	    let
		val nodeInfo = G.nodeInfo hdNode
		val nodeDegree = G.degree hdNode
		val k = List.length regs
	    in
		if isPrecolored nodeInfo
		then
		    simplify (graph, tlNodes)
		else
		    (* Check if degree < k *)
		    if nodeDegree < k
		    then let val nid = G.getNodeID hdNode
			     val graph' = G.removeNode(graph, nid)
			 in (graph', SOME(nid)) end
		    else simplify (graph, tlNodes)
	    end

    in
	simplify (interference, G.nodes interference)
    end;

end

structure RegAlloc : REG_ALLOC =
struct
structure Frame : FRAME = MipsFrame
type allocation = Frame.register Temp.Table.table

fun alloc () = ();

end

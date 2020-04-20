structure Color : COLOR =
struct

structure Frame : FRAME = MipsFrame
type allocation = Frame.register Temp.Map.map (* Updated temp *)

structure TempSet = Temp.Set
structure TempMap = Temp.Map

(* allocation * Temp.temp list *)
(* returns allocation=(temp->reg map) * spilled temps *)
fun color {interference = Liveness.igraph{graph, tnode, moves}, initial = init, spillCost = spillCost, registers = regs} =
    let
	val alloc : (Frame.register Temp.Map.map) = initial
	val spilledTemps : (Temp.temp list) = []
	val colors : Frame.register list = regs

	(* Returns new graph and node that can be colored *)
	(* (graph * nodelist) -> (graph * nid option) *)
	fun simplify (graph, []) = (graph, NONE)
	  | simplify (graph, hdNode::tlNodes) = 
	    let
		val temp = G.nodeInfo hdNode
		val nodeDegree = G.degree hdNode
		val k = List.length regs

		fun isPrecolored temp' =
		    case TempMap.find (initial, temp') of
			SOME x => true
		      | NONE => false

	    in if isPrecolored temp
	       then simplify (graph, tlNodes)
	       else if nodeDegree < k
	       then let val nid = G.getNodeID hdNode
			val graph' = G.removeNode(graph, nid)
		    in (graph', SOME nid) end (* returns the first simple node *)
	       else simplify (graph, tlNodes)
	    end

	fun assign_color graph' =
	    let val (graph'', nidopt) = simplify (graph', G.nodes graph')
	    in case nidopt of
		   SOME nid =>
		   let val (alloc, spilledTemps) = assign_color graph''
		       val temp = G.nodeInfo nid
		   in
		       case colors of
			   [] => (alloc, temp::spilledTemps) (* actual spill - insufficient regs *)
			 | reg::l=> 
			   (TempMap.insert (alloc, temp, reg); (* assign color *)
			    List.drop (colors, 1); (* remove color *)
			    (alloc, spilledTemps))
		   end
		 | NONE => (init, spilledTemps)
		   (* either temps spilled or we're done - assume done *)
    in
	assign_color interference; (* do reg alloc *)
	(alloc, spilledTemps) (* return the result *)
    end;

end

structure RegAlloc : REG_ALLOC =
struct
structure Frame : FRAME = MipsFrame
type allocation = Frame.register Temp.Map.map

fun alloc () = ();

end

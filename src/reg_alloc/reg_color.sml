(* structure GL = FuncGraph(struct *)
(* 			  type ord_key = temp *)
(* 			  val compare = Int.compare *)
(* 			  end) *)

structure Color :
	  sig
	      
	      type allocation = string Temp.Table.table
	      val color: {interference: Liveness.igraph,
			  initial: allocation,
			  spillCost: unit GL.node -> int,
			  registers: string list}
			 -> allocation * Temp.temp list
						   (* returns allocation=(temp->reg map) * spilled temps *)
	  end =
struct

type allocation = string Temp.Table.table (* Updated temp *)

(*
structure TempSet = Temp.Set
structure TempMap = Temp.Map
*)

fun member (ls : string list, str : string) = List.length(List.filter (fn s => s = str) ls) > 0

												  
(* allocation * Temp.temp list *)
(* returns allocation=(temp->reg map) * spilled temps *)
fun color ({interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, registers}) =
    let val
	filterPreColored = List.filter (fn node =>
					   case Temp.Table.look(initial, gtemp node) of
					       SOME(t) =>  not(member(registers, t))
					     | NONE => true) (GL.nodes graph)
    in
	case filterPreColored of
	    [] => (print("interference graph has no nodes"); (initial, []))
	  | [node] =>
	    let val neighborsNotSpilled =
		    List.filter (fn t =>
				    case Temp.Table.look(initial, t) of
					SOME(e) => true
				      | NONE => false) (GL.preds node)
		val colorNeighbors =
		    map (fn t => case Temp.Table.look(initial, t) of
				     SOME(e) => e
				   | NONE => (print("neighbor is not spilled and also not colored"); "")) neighborsNotSpilled
		val colorsAvailable = List.filter(fn reg => not(member(colorNeighbors, reg))) registers
	    in
		if List.length(colorsAvailable) > 0
		then (Temp.Table.enter(initial, gtemp(node), List.hd(colorsAvailable)), [])
		else (initial, [gtemp(node)])
	    end
	  | list => let val notSigNodes = List.filter (fn node => GL.outDegree(node) < List.length(registers)) list
			val nextToSimplify = if List.length(notSigNodes) = 0
					     then List.hd(list)
					     else List.hd(notSigNodes)
			val updatedGraph = GL.remove(graph, nextToSimplify)
			val updatedInter = Liveness.IGRAPH{graph = updatedGraph, tnode = tnode, gtemp = gtemp, moves = moves}
		    in
			let val (updatedAlloc, spillList) = color({interference = updatedInter, initial = initial, spillCost = spillCost, registers = registers})
			    val neighborsNotSpilled = List.filter (fn t => case Temp.Table.look(updatedAlloc, t) of
									       SOME(e) => true
									     | NONE => false) (GL.preds(nextToSimplify))
			    val colorNeighbors = map (fn t => case Temp.Table.look(updatedAlloc, t) of
								  SOME(e) => e
								| NONE => (print("neighbor is not spilled and also not colored"); "")) neighborsNotSpilled
			    val colorsAvailable = List.filter(fn reg => not(member(colorNeighbors, reg))) registers
			in
			    if List.length(colorsAvailable) > 0
			    then (Temp.Table.enter(updatedAlloc, gtemp(nextToSimplify), (List.hd(colorsAvailable))), spillList)
			    else (updatedAlloc, spillList @ [gtemp(nextToSimplify)] )
				     
			end
		    end
    end
(*
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


	    end*)
end

structure GL = FuncGraph(struct
			  type ord_key = temp
			  val compare = Int.compare
			  end)

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

fun member (x, nil) = false
  | member (x, y::ys) = x=y orelse member (x,ys); 

			 
(* allocation * Temp.temp list *)
(* returns allocation=(temp->reg map) * spilled temps *)
fun color ({interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial = init, spillCost = spillCost, registers = regs}) =
    let val filterPreColored = List.filter (fn node => case Temp.Table.look(init, gtemp node) of SOME(t) => not(member(regs, t))
													| NONE => true) GL.nodes(graph) 
    in
	case filterPreColored of [] => (print("interference graph has no nodes"); (init, []))
			       | [node] => let val neighborsNotSpilled = List.filter (fn t => Temp.Table.look(init, t) of SOME(e) => true
															| NONE => false) GL.preds(node)
					       val colorNeighbors = map (fn t => case Temp.Table.look(init, t) of SOME(e) => e
														| NONE => (print("neighbor is not spilled and also not colored"))) neighborsNotSpilled
					       val colorsAvailable = List.filter(fn reg => not(member(colorNeighbors, reg))) regs
					   in
					       if List.length(colorsAvailable) > 0
					       then (Temp.Table.enter(init, gtemp(node), List.nth(colorsAvailable, 0)), [])
					       else (init, [gtemp(node)])
					   end
			       | list => let val notSigNodes = List.filter (fn node => GL.outDegree(node) < List.length(regs)) list
					     val nextToSimplify = if List.length(notSigNodes) = 0
								  then List.hd(list)
								  else List.hd(notSigNodes)
					     val updatedGraph = GL.remove(graph, nextToSimplify)
					     val updatedInter = Liveness.IGRAPH{graph = updatedGraph, tnode = tnode, gtemp = gtemp, moves = moves}
					 in
					     let val (updatedAlloc, spillList) = color({interference = updatedInter, initial = init, spillCost = spillCost, registers = regs})
						 val neighborsNotSpilled = List.filter (fn t => Temp.Table.look(updatedAlloc, t) of SOME(e) => true
															  | NONE => false) GL.preds(nextToSimplify)
						 val colorNeighbors = map (fn t => case Temp.Table.look(updatedAlloc, t) of SOME(e) => e
														| NONE => (print("neighbor is not spilled and also not colored"))) neighborsNotSpilled
						 val colorsAvailable = List.filter(fn reg => not(member(colorNeighbors, reg))) regs
					     in
						 if List.length(colorsAvailable) > 0
						 then (Temp.Table.enter(updatedAlloc, gtemp(nextToSimplify), List.hd(colorsAvailable)), spillList)
						 else (updatedAlloc, spillList @ [gtemp(nextToSimplify)]
						     
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

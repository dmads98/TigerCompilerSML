signature COLOR = 
sig
    structure Frame : FRAME
    type allocation = string Temp.Table.table
    val color: {interference: Liveness.igraph,
		initial: allocation,
		spillCost: unit LG.node -> int,
		registers: string list}
	       -> allocation * Temp.temp list
					 
end

structure Color : COLOR = 
struct

type allocation = string Temp.Table.table (* Updated temp *)


fun isPartOfMove (_, []) = false
  | isPartOfMove (temp, (srcNode, dstNode) :: list) =
    if temp = LG.getNodeID(srcNode) orelse temp = LG.getNodeID(dstNode)
    then true
    else isPartOfMove(temp, list)

fun getNextToSimplify ([], _, _, _) = NONE
  | getNextToSimplify (curNode :: remaining, initialAlloc, moveList, numRegs) =
    let val curId = LG.getNodeId(curNode)
	val deg = LG.outDegree(curNode)
    in
	if deg < numRegs
	   andalso (not isSome(Temp.Table.look(initialAlloc, curId)))
	   andalso (not isPartOfMove(curId, moveList))
	then SOME(curId)
	else getNextToSimplify(remaining, initialAlloc, moveList, numRegs)
    end

fun getColor (_, _, _, []) = NONE
  | getColor (id, graph, curAlloc, curColor::list) =
    let fun checkNeighbors (neighbor, state) = state orelse
					       case Temp.Table.look(curAlloc, neighbor) of
						   SOME(neighborColor) => (curColor = neighborColor)
						 | NONE => false
	val failedColoring = foldl checkNeighbors false (LG.adj(LG.getNode(graph, id)))
    in
	if failedColoring
	then getColor(id, graph, curAlloc, list)
	else SOME(curColor)
    end

fun getSpillNodes (curAlloc, graph) =
    let fun isPossibleSpillNode (curNode, list) =
	    if (isSome(Temp.Table.look(curAlloc, LG.getNodeID(curNode))))
	    then list
	    else (LG.getNodeID(curNode)::list)
    in
	foldl isPossibleSpillNode [] (LG.nodes(graph))
    end

fun getCoalescedColor (_, _, _, _, []) = "REG NOT FOUND"
  | getCoalescedColor (temp1, temp2, graph, curAlloc, curColor::list) =
    let fun checkNeighbors (neighbor, state) = state orelse
					       case Temp.Table.look(curAlloc, neighbor) of
						   SOME(neighborColor) => (curColor = neighborColor)
						 | NONE => false
	val failedColoring = foldl checkNeighbors false (LG.adj(LG.getNode(graph, temp1)) @
							LG.adj(LG.getNode(graph, temp2)))
    in
	if failedColoring
	then getCoalescedColor(temp1, temp2, graph, curAlloc, list)
	else curColor
    end

(* allocation * Temp.temp list *)
(* returns allocation=(temp->reg map) * spilled temps *)
fun color ({interference as Liveness.IGRAPH{graph, tnode, gtemp, moves}, initial, spillCost, registers}) =
    let val nextToSimplify = getNextToSimplify(LG.nodes(graph), initial, moves, List.length(registers))
    in
	case nextToSimplify of
	    SOME(simpId) =>
	    let val updatedGraph = LG.removeNode(graph, simpId)
		val (newAlloc, spillList) = color({Liveness.IGRAPH{graph = updatedGraph,
								   tnode = tnode,
								   gtemp = gtemp,
								   moves = moves},
						   initial = initial,
						   spillLost = spillCost,
						   registers = registers})
	    in
		case getColor(simpId, graph, newAlloc, registers) of
		    SOME(col) => (Temp.Table.enter(newAlloc, simpId, col), spillList)
		 | NONE => (newAlloc, simpId::spillList)
	    end
	  | NONE =>
	    if List.length(moves) = 0
	    then (initial, getSpillNodes(initial, graph))
	    else
		let val (srcNode, dstNode) = List.hd(moves)
		    val srctemp = LG.getNodeID(srcNode)
		    val dsttemp = LG.getNodeID(dstNode)
		in
		    if (isSome(Temp.Table.look(initial, srctemp)))
		       andalso (not (isSome(Temp.Table.look(initial, dsttemp))))
		       andalso (LG.inDegree(srcNode) + LG.inDegree(dstNode) < 30)
		    then color({Liveness.IGRAPH{graph = graph,
						tnode = tnode,
						gtemp = gtemp,
						moves = List.drop(moves, 1)},
				initial = Temp.Table.enter(initial, dsttemp, valOf(Temp.Table.look(initial, srctemp))),
				spillLost = spillCost,
				registers = registers})
		    else if (not isSome(Temp.Table.look(initial, srctemp)))
			    andalso (isSome(Temp.Table.look(initial, dsttemp)))
			    andalso (LG.inDegree(srcNode) + LG.inDegree(dstNode) < 30)
		    then color({Liveness.IGRAPH{graph = graph,
						tnode = tnode,
						gtemp = gtemp,
						moves = List.drop(moves, 1)},
				initial = Temp.Table.enter(initial, srctemp, valOf(Temp.Table.look(initial, dsttemp))),
				spillLost = spillCost,
				registers = registers})
		    else if (not isSome(Temp.Table.look(initial, srctemp)))
			    andalso (not isSome(Temp.Table.look(initial, dsttemp)))
			    andalso (LG.inDegree(srcNode) + LG.inDegree(dstNode) < 30)
		    then
			let val assignedColor = getCoalescedColor(srctemp, dsttemp, graph, initial, registers)
			    val updatedAlloc = Temp.Table.enter(
				    Temp.Table.enter(initial, srctemp, assignedColor),
				    dsttemp, assignedColor)
			in
			    color({Liveness.IGRAPH{graph = graph,
						tnode = tnode,
						gtemp = gtemp,
						moves = List.drop(moves, 1)},
				initial = updatedAlloc,
				spillLost = spillCost,
				registers = registers})
			end
		    else color({Liveness.IGRAPH{graph = graph,
						tnode = tnode,
						gtemp = gtemp,
						moves = List.drop(moves, 1)},
				initial = initial,
				spillLost = spillCost,
				registers = registers})
		end
	
    end
end

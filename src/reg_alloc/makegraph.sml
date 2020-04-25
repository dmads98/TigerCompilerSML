structure MakeGraph : MAKEGRAPH =
struct

structure A = Assem

val index = ref 0

type data = {defs: Temp.temp list,
	     uses: Temp.temp list,
	     isMove: bool}

structure labelOrdMap = SplayMapFn(struct
				    type ord_key = string
				    val compare = String.compare
				    end)

val labelInstrMap : (int labelOrdMap.map ref) = ref labelOrdMap.empty

fun getAndUpdateIndex () =
    let val idx = !index
    in
	index := idx + 1;
	idx
    end

fun instrs2graph instrList =
    let fun createNode (A.OPER{assem, dst, src, jump}, curGraph) =
	    let val id = getAndUpdateIndex()
		val nodeData = {defs = dst,
				uses = src,
				isMove = false}
	    in
		FG.addNode(curGraph, id, nodeData)
	    end
	  | createNode (A.LABEL{assem, lab}, curGraph) =
	    let val id = getAndUpdateIndex()
		val nodeData = {defs = [],
				uses = [],
				isMove = false}
	    in
		labelInstrMap := labelOrdMap.insert(!labelInstrMap, Symbol.name(lab), id);
		FG.addNode(curGraph, id, nodeData)
	    end
	  | createNode (A.MOVE{assem, dst, src}, curGraph) =
	    let val id = getAndUpdateIndex()
		val nodeData = {defs = [dst],
				uses = [src],
				isMove = true}
	    in
		FG.addNode(curGraph, id, nodeData)
	    end

	fun addEdges (graph, instrList) =
	    let fun edgeHelper (_, [], graph) = graph
		  | edgeHelper (SOME predId, insn::rest, graph) =
		    let val id = getAndUpdateIndex()
		    in
			case insn of
			    A.OPER {assem, dst, src, jump = NONE} =>
			    let val updatedGraph = FG.addEdge(graph, {from = predId, to = id})
			    in
				edgeHelper(SOME(id), rest, updatedGraph)
			    end
			  | A.OPER {assem, dst, src, jump = SOME labs} =>
			    let val edges = foldl (fn (lab, ls) => ls @ [{to = valOf(labelOrdMap.find(!labelInstrMap, Symbol.name(lab))), from = id}]) [] labs
				val updatedGraph = foldl (fn (e, g) => FG.addEdge(g, e)) graph edges
				val updatedGraph' = FG.addEdge(updatedGraph, {from = predId, to = id})
			    in
				edgeHelper(NONE, rest, updatedGraph')
			    end
			  | A.LABEL{assem, lab} =>
			    let val updatedGraph = FG.addEdge(graph, {from = predId, to = id})
			    in
				edgeHelper(SOME(id), rest, graph)
			    end
			  | A.MOVE{assem, dst, src} =>
			    let val updatedGraph = FG.addEdge(graph, {from = predId, to = id})
			    in
				edgeHelper(SOME(id), rest, graph)
			    end
			
		    end
		  | edgeHelper (NONE, insn::rest, graph) =
		    let val id = getAndUpdateIndex()
		    in
			case insn of
			    A.OPER {assem, dst, src, jump = NONE} => edgeHelper(SOME(id), rest, graph)
			  | A.OPER {assem, dst, src, jump = SOME labs} =>
			    let val edges = foldl (fn (lab, ls) => ls @ [{to = valOf(labelOrdMap.find(!labelInstrMap, Symbol.name(lab))), from = id}]) [] labs
				val updatedGraph = foldl (fn (e, g) => FG.addEdge(g, e)) graph edges
			    in
				edgeHelper(NONE, rest, updatedGraph)
			    end
			  | A.LABEL{assem, lab} => edgeHelper(SOME(id), rest, graph)
			  | A.MOVE{assem, dst, src} => edgeHelper(SOME(id), rest, graph) 
		    end
	    in
		edgeHelper(NONE, instrList, graph)
	    end

(*	fun createEdge (instr as A.MOVE{assem, dst, src}, curGraph) =
	    let val edge = {to = !index + 1, from = !index}
	    in
		index := !index + 1;
		if !index < List.length(G.nodes(curGraph))
		then G.addEdge(curGraph, edge)
		else curGraph
	    end
	  | createEdge (instr as A.LABEL{assem, lab}, curGraph) =
	    let val edge = {to = !index + 1, from = !index}
	    in
		index := !index + 1;
		if !index < List.length(G.nodes(curGraph))
		then G.addEdge(curGraph, edge)
		else curGraph
	    end
	  | createEdge (instr as A.OPER{assem, dst, src, jump}, curGraph) =
	    let val edges = case jump of SOME ([]) => []
				       | SOME(labelList) => foldl (fn (lab, ls) => ls @ [{to = valOf(labelOrdMap.find(!labelInstrMap, Symbol.name(lab))), from = !index}]) [] labelList
				       | NONE => if !index + 1 < List.length(G.nodes(curGraph))
						 then [{to = !index + 1, from = !index}]
						 else []
	    in
		index := !index + 1;
		foldl (fn (e, g) => G.addEdge(g, e)) curGraph edges
	    end*)

	val nodeGraph = (index := 0; foldl createNode FG.empty instrList)
	val completeGraph = (index := 0; addEdges(nodeGraph, instrList))
    in
	completeGraph
    end
end

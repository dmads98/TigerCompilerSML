structure G = FuncGraph(struct
			 type ord_key = int
			 val compare = Int.compare
			 end)

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

fun getAndUpdateIndex instr =
    let val idx = !index
    in
	index := idx + 1;
	idx
    end

fun instrs2graph instrList =
    let fun createNode (instr as A.OPER{assem, dst, src, jump}, curGraph) =
	    let val id = getAndUpdateIndex instr
		val nodeData = {defs = dst,
				uses = src,
				isMove = false}
	    in
		G.addNode(curGraph, id, nodeData)
	    end
	  | createNode (instr as A.LABEL{assem, label}, curGraph) =
	    let val id = getAndUpdateIndex instr
		val nodeData = {defs = [],
				uses = [],
				isMove = false}
	    in
		labelInstrMap := labelOrdMap.insert(!labelInstrMap, Symbol.name(label), id);
		G.addNode(curGraph, id, nodeData)
	    end
	  | createNode (instr as A.MOVE{assem, dst, src}, curGraph) =
	    let val id = getAndUpdateIndex instr
		val nodeData = {defs = [dst],
				uses = [src],
				isMove = true}
	    in
		G.addNode(curGraph, id, nodeData)
	    end

	fun createEdge (instr as A.MOVE{assem, dst, src}, curGraph) =
	    let val edge = {to = !index + 1, from = !index}
	    in
		index := !index + 1;
		if index < List.length(G.nodes(curGraph))
		then G.addEdge(curGraph, edge)
		else curGraph
	    end
	  | createEdge (instr as A.LABEL{assem, label}, curGraph) =
	    let val edge = {to = !index + 1, from = !index}
	    in
		index := !index + 1;
		if index < List.length(G.nodes(curGraph))
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
	    end

	val nodeGraph = (index := 0; foldl createNode G.empty instrList)
	val completeGraph = (index := 0; foldl createEdge nodeGraph instrList)
    in
	(completeGraph, G.node(completeGraph))
    end
end

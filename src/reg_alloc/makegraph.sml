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
				edgeHelper(SOME(id), rest, updatedGraph)
			    end
			  | A.MOVE{assem, dst, src} =>
			    let val updatedGraph = FG.addEdge(graph, {from = predId, to = id})
			    in
				edgeHelper(SOME(id), rest, updatedGraph)
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

	val nodeGraph = (index := 0; foldl createNode FG.empty instrList)
	val completeGraph = (index := 0; addEdges(nodeGraph, instrList))
    in
	completeGraph
    end

fun printNodeFG (id, {defs, uses, isMove}) =
    let (* val {defs, uses, isMove} = FG.nodeInfo node *)
	fun temp_str (temp, prev_str) = prev_str ^   ", " ^ Temp.makestring temp
	val defs_str = foldl temp_str "" defs
	val uses_str = foldl temp_str "" uses
	val isMove_str = case isMove of
			     true => ", has Move"
			   | false => ""
    in
	(* "---" ^ *)
	"{ID: " ^ (Int.toString id) ^ " , Defs: " ^ defs_str ^ ", Uses:" ^ uses_str  ^ isMove_str ^ "}"
	(* ^ "--" *)
    end
	
fun show flowgraph = FG.printGraph printNodeFG flowgraph;

end

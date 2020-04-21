structure Reg_Alloc:
sig
    structure Frame : FRAME
    type allocation = string Temp.Table.table
    val alloc : Assem.instr list * Frame.frame ->
		Assem.instr list * allocation
end
=
struct

structure Frame = MipsFrame
type allocation = string Temp.Table.table

fun handleSpills (alloc, calleeSaves, instrList, [], cur) = (alloc, cur, instrList)
  | handleSpills (alloc, [], instrList, spillList, cur) = (ErrorMsg.error ~1 "Failed to allocate registers for file"; (alloc, cur, instrList))
  | handleSpills (alloc, reg::calleeSaves, instrList, spillList, cur) =
    let val sinkInstr = Assem.OPER{assem = "",
				  src = (Frame.getSinks() @ calleeSaves),
				  dst = [],
				  jump = SOME([])}
	val updatedInstrs = List.take(instrList, List.length(instrList) - 1) @ [sinkInstr]
	val (fg, nodeList) = MakeGraph.instrs2graph(updatedInstrs)
	val (ig, _) = Liveness.interferenceGraph(fg)
	val (updatedAlloc, updatedSpills) = Color.color({interference = ig,
							 initial = Frame.getPreColoredAlloc(),
							 spillCost = (fn r => 1),
							 registers = Frame.getAllRegs()})
    in
	handleSpills(updatedAlloc, calleeSaves, updatedInstrs, updatedSpills, cur @ [reg])
    end

	
fun alloc (instrList, frame) =
    let val (fg, nodeList) = MakeGraph.instrs2graph(instrList)
	val (igraph, getLiveOut) = Liveness.interferenceGraph(fg)
	val (alloc, spillList) = Color.color({intereference = igraph,
					      initial = Frame.getPreColoredAlloc(),
					      spillCost = (fn t => 1),
					      registers = Frame.getAllRegs()})
	val (fAlloc, frameSpills, fInstrs) = handleSpills(alloc, Frame.getCalleeSaves(), instrList, spillList, [])
	val finalInstrs = #body(Frame.procEntryExit3(frame, fInstrs, frameSpills))
    in
	(finalInstrs, fAlloc)
    end
end

signature REG_ALLOC =
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Table.table
    val alloc : Assem.instr list * Frame.frame ->
		Assem.instr list * allocation
end

signature COLOR =
sig
    structure Frame : FRAME
    type allocation = Frame.register Temp.Table.table
    val color: {interference: Liveness.igraph,
		initial: allocation,
		spillCost: Graph.node -> int,
		registers: Frame.register list}
	       -> allocation * Temp.temp list
end

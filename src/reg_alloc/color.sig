signature COLOR =
sig
    
    type allocation = string Temp.Table.table
    val color: {interference: Liveness.igraph,
		initial: allocation,
		spillCost: unit .node -> int,
		registers: Frame.register list}
	       -> allocation * Temp.temp list;
    (* returns allocation=(temp->reg map) * spilled temps *)
end

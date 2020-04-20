structure G = FuncGraph(struct
			 type ord_key = int
			 val compare = Int.compare
			 end)

signature MAKEGRAPH =
sig
    type data
    val instrs2graph : Assem.instr list -> data G.graph * data G.node list
end

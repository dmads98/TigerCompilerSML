structure FG = FuncGraph(struct
			 type ord_key = int
			 val compare = Int.compare
			 end)
signature MAKEGRAPH =
sig

    type data
    val instrs2graph : Assem.instr list -> data FG.graph
end

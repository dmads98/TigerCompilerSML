structure G = FuncGraph(struct
			 type ord_key = temp
			 val compare = Int.compare
			 end)

signature LIVENESS =
sig
    type data
    datatype igraph =
	     IGRAPH of {graph: unit G.graph,
			tnode: Temp.temp -> unit G.node,
			gtemp: unit G.node -> Temp.temp,
			moves: (unit G.node * unit G.node) list}

    type livenessData

    val interferenceGraph : data G.graph -> igraph * (data G.node -> Temp.temp list)
(*    val show : TextIO.outstream * igraph -> unit*)
end

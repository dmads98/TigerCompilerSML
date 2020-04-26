structure LG = FuncGraph(struct
			  type ord_key = Temp.temp
			  val compare = Temp.compare
			  end)
			
		       
signature LIVENESS =
sig
    type data
    datatype igraph =
	     IGRAPH of {graph: unit LG.graph,
			tnode: Temp.temp -> unit LG.node,
			gtemp: unit LG.node -> Temp.temp,
			moves: (unit LG.node * unit LG.node) list}

    type livenessData

    val interferenceGraph : data FG.graph -> igraph
    val show : igraph -> unit
end

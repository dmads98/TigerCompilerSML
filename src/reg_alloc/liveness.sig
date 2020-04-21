structure G = FuncGraph(struct
			 type ord_key = int
			 val compare = Int.compare
			 end)
structure GL = FuncGraph(struct
			  type ord_key = temp
			  val compare = Int.compare
			  end)
			
		       
signature LIVENESS =
sig
    type data
    datatype igraph =
	     IGRAPH of {graph: unit GL.graph,
			tnode: Temp.temp -> unit GL.node,
			gtemp: unit GL.node -> Temp.temp,
			moves: (unit GL.node * unit GL.node) list}

    type livenessData

    val interferenceGraph : data G.graph -> igraph * (data G.node -> Temp.temp list)
(*    val show : TextIO.outstream * igraph -> unit*)
end

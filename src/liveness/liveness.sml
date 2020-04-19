structure Liveness : LIVENESS =
struct

(* DS to remember what is live at end of each flow graph *)
type liveSet = unit Temp.Table.table * temp list
type liveMap = liveSet Flow.Graph.Table.table


datatype igraph =
	 IGRAPH of {graph: IGraph.graph,
		    tnode: Temp.temp -> IGraph.node,
		    gtemp; IGraph.node -> Temp.temp,
			   moves: (IGraph.node * IGraph.node) list}

fun interpherenceGraph () = ();

fun show () = ();

end

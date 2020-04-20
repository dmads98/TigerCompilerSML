structure Color : COLOR =
struct

structure Frame : FRAME = MipsFrame
type allocation = Frame.register Temp.Table.table

structure TempSet = Temp.Set
structure TempMap = Temp.Map

fun color {interference = Liveness.igraph{graph, tnode, moves}, initial = init, spillCost = spillCost, registers = regs} =
    let
	(* isSome, valOf can be used to check precolor *)
	fun simplify () = ();
	
	fun select (graph, nid, alloc) = ();

    in
    end;

end

structure Color : COLOR =
struct

structure Frame : FRAME = MipsFrame
type allocation = Frame.register Temp.Table.table

fun color {interference = Liveness.igraph{graph, tnode, moves}, initial = init, spillCost = spillCost, registers = regs} =
    let
	(* isSome, valOf can be used to check precolor *)

    in
    end;

end

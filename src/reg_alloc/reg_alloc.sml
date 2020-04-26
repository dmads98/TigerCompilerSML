signature REG_ALLOC = 
sig
    structure Frame : FRAME
    type allocation = string Temp.Table.table
    val alloc : Liveness.igraph ->
    		allocation * bool
    val printAlloc : string Temp.Table.table * Temp.temp list -> unit
end

structure Reg_Alloc : REG_ALLOC = 
struct

structure Frame = MipsFrame
type allocation = string Temp.Table.table

fun printAlloc (alloc, tList) =
    let fun printTemp(temp) = case Temp.Table.look(alloc, temp) of
				  SOME(x) => print(Temp.makestring(temp) ^ " -> " ^ x ^ "\n")
				| NONE => print("===Unable to allocate: " ^ Temp.makestring(temp) ^ "===\n")
    in
	app printTemp tList
    end
			 
val initAlloc =
    let fun addReg ((reg, strReg), curTable) = Temp.Table.enter(curTable, reg, strReg)
    in
	foldl addReg Temp.Table.empty (Frame.callersaves @ Frame.calleesaves @ Frame.argregs @ Frame.specialregs)
    end

val registers =
    let fun addReg ((reg, strReg), ls) = ls @ [strReg]
    in
	foldl addReg [] (Frame.callersaves @ Frame.calleesaves @ Frame.argregs @Frame.specialregs)
    end

fun alloc (igraph) =
    let val (updatedAlloc, spillList) = Color.color ({interference = igraph, initial = initAlloc, spillCost = (fn x => 1), registers = registers})
    in
	(updatedAlloc, List.length(spillList) > 0)
    end
end

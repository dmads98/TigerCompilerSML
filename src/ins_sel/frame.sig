signature FRAME =
sig
    type access
    type frame

    val wordSize: int
    val FP : Temp.temp
    val V0 : Temp.temp
    val SP : Temp.temp
    val RA : Temp.temp

    val string : Tree.label * string -> string (* MIPS format string *)
    val allocLocal : frame -> bool -> access
    val name : frame -> Temp.label
    val formals : frame -> access list
    val newFrame : {name: Temp.label, formals : bool list} -> frame
    val exp : access -> Tree.exp -> Tree.exp
(*    val procEntryExit1 : frame -> Tree.stm -> Tree.stm*)
    val procEntryExit2 : frame * Assem.instr list -> Assem.instr list
    val procEntryExit3 : frame * Assem.instr list -> {prolog: string, body : Assem.instr list, epilog: string}

    val externalCall : string * Tree.exp list -> Tree.exp
						  
    datatype frag = PROC of {body: Tree.stm, frame: frame}
		  | STRING of Temp.label * string

    val tempMap : string Temp.Table.table
    val getRegName : Temp.temp -> string

    val getCalleeSaves : unit -> Temp.temp list
    val getCallerSaves : unit -> Temp.temp list
    val getArgRegs : unit -> Temp.temp list
    val getReturnRegisters : unit -> Temp.temp list
    val getReturnAddress : unit -> Temp.temp
    
end

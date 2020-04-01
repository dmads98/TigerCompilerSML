signature FRAME =
sig
    type access
    type frame

    val wordSize: int
    val FP : Temp.temp
    val RV : Temp.temp

    val allocLocal : frame -> bool -> access
    val name : frame -> Temp.label
    val formals : frame -> access list
    val newFrame : {name: Temp.label, formals : bool list} -> frame
    val exp : access -> Tree.exp -> Tree.exp
    val procEntryExit1 : frame -> Tree.stm -> Tree.stm

    val externalCall : string * Tree.exp list -> Tree.exp
						  
    datatype frag = PROC of {body: Tree.stm, frame: frame}
		  | STRING of Temp.label * string
    
end

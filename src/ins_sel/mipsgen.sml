structure MipsGen : CODEGEN =
struct

structure T = Tree
structure A = Assem

fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
    let val ilist = ref (nil: A.instr list)
	fun emit x = ilist := x :: !ilist
	fun result (gen) = let val t = Temp.newtemp() in gen t; t end
	fun int (x: int) =
	    if (x>=0) then Int.toString x
	    else "-" ^ Int.toString (~x)

	fun munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)
	  (* label *)
	  | munchStm (T.LABEL label) =
	    emit(A.LABEL{assem=label ^ ":\n", lab=label})
		
	  (* sw *)
	  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
	    (A.OPER{assem = "sw 'd0, " ^ int i ^ "('s0)",
		    src = [munchexp e1, munchexp e2],
		    dst = [], jump = NONE
	    })
	  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
	    (A.OPER{assem = "sw 'd0, " ^ int i ^ "('s0)",
		    src = [munchexp e1, munchexp e2],
		    dst = [], jump = NONE
	    })
	  | munchStm (T.MOVE(T.MEM(T.CONST i), e2)) =
	    (A.OPER{assem = "sw 'd0, " ^ int i ^ "('s0)",
		    src = [munchexp e2],
		    dst = [], jump = NONE
	    })
	  | munchStm (T.MOVE(T.MEM(e1), e2)) =
	    (A.OPER{assem = "sw 'd0, " ^ int i ^ "('s0)",
		    src = [munchexp e1, munchexp e2],
		    dst = [], jump = NONE
	    })
	  | munchStm (T.MOVE(T.TEMP i, e2)) = (* add 0 to 's0 -> 'd0*)
	    (A.OPER{assem = "add 'd0, " ^ int i ^ "('zero)",
		    src = [munchexp e2],
		    dst = [], jump = NONE
	    })

	and munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "lw 'd0, " ^ int i ^ "('s0)",
				src = [munchExp e1],
				dst = [r],
				jump = NONE
		  }))
	  | munchExp  = 
	    result(fn r =>
		      emit(A.OPER{
				assem = "lw 'd0, " ^ int i ^ "('s0)",
				src = [munchExp e1],
				dst = [r],
				jump = NONE
		  }))
		  
    in munchStm stm;
       rev(!ilist)
    end
end

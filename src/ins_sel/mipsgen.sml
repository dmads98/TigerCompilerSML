structure MipsGen : CODEGEN =
struct

structure T = Tree
structure A = Assem
structure S = Symbol
structure F = MipsFrame
		  
fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
    let val ilist = ref (nil: A.instr list)
	fun emit x = ilist := x :: !ilist
	fun result (gen) = let val t = Temp.newtemp() in gen t; t end
	fun int (x: int) =
	    if (x>=0) then Int.toString x
	    else "-" ^ Int.toString (~x)

	fun getString (T.PLUS) = "add"
	  | getString (T.MINUS) = "sub"
	  | getString (T.MUL) = "mul"
	  | getString (T.DIV) = "div"
	  | getString (T.EQ) = "beq"
	  | getString (T.NE) = "bne"
	  | getString (T.LT) = "blt"
	  | getString (T.GT) = "bgt"
	  | getString (T.LE) = "ble"
	  | getString (T.GE) = "bge"
				    
	fun munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)
	  (* label *)
	  | munchStm (T.LABEL label) =
	    emit(A.LABEL{assem = S.name label ^ ":\n", lab=label})
		
	  | munchStm (T.CJUMP(oper, e1, e2, tl, fl)) =
	    emit (A.OPER{assem = getString(oper) ^ " `s0, `s1, " ^ S.name(tl) ^ " \n",
			 src = [munchExp e1, munchExp e2],
			 dst = [],
			 jump = SOME([tl, fl,])})
	  | munchStm (T.JUMP(T.NAME lab, labs)) =
	    emit (A.OPER{assem = "j " ^ S.name(lab) ^ " \n",
			 src = [],
			 dst = [],
			 jump = SOME(labs)})
		 
	  (* sw *)
	  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
	    emit (A.OPER{assem = "sw 's0, " ^ int i ^ "(`s1) \n",
			 src = [munchexp e2, munchexp e1],
			 dst = [], jump = NONE})
	  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
	    emit (A.OPER{assem = "sw 's0, " ^ int i ^ "(`s1) \n",
			 src = [munchexp e2, munchexp e1],
			 dst = [], jump = NONE})
	  | munchStm (T.MOVE(T.MEM(e1), e2)) =
	    emit (A.OPER{assem = "sw `s0, 0(`s1) \n ",
			 src = [munchexp e2, munchexp e1],
			 dst = [], jump = NONE})
	  | munchStm (T.MOVE(T.TEMP i, e2)) =
	    emit (A.MOVE{assem = "move `d0, `s0\n",
			 src = munchexp e2,
			 dst = i})
	  | munchStm (T.EXP e) = (munchExp e; ())
	  | munchStm _ = (ErrorMsg.error ~1 "Error while generating assembly"; ()) 
			     
	and munchExp (T.CALL(T.NAME name, argList)) =
	    let val frameOffset = if List.length argList > 4
			      then (List.length(argList) - 4) * ~4
			      else 0
		val spMove = T.MOVE(T.TEMP F.SP, T.BINOP(T.PLUS, T.TEMP, F.SP, T.CONST frameOffset))
		val spReturn = T.MOVE(T.TEMP F.SP, T.BINOP(T.PLUS, T.TEMP, F.SP, T.CONST(~1 * frameOffset)))
	    in
		(munchStm spMove;
		 (emit (A.OPER{
			     assem = "jal " ^ S.name name ^ "\n",
			     src = munchArgs(0, argList),
			     dst = F.getCallerSaves() @ F.getReturnRegisters() @ [F.getReturnAddress()] @ F.getArgRegs(),
			     jump = NONE}));
		 munchStm spReturn;
		 F.V0)
					    
	    end
	  | munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "lw `d0, " ^ int i ^ "('s0)\n",
				src = [munchExp e1],
				dst = [r],
				jump = NONE
		  }))
	  | munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "lw `d0, " ^ int i ^ "('s0)\n",
				src = [munchExp e1],
				dst = [r],
				jump = NONE
		  }))
	  | munchExp (T.MEM(e1)) = 
	    result(fn r =>
		      emit(A.OPER{
				assem = "lw `d0, 0(`s0)\n",
				src = [munchExp e1],
				dst = [r],
				jump = NONE
		  }))		  
	  | munchExp (T.BINOP(T.PLUS, e1, T.CONST c)) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "addi `d0, `s0, " ^ int i ^ "\n",
				src = [munchExp e1],
				dst = [r],
				jump = NONE}))
	  | munchExp (T.BINOP(T.PLUS, T.CONST c, e1)) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "addi `d0, `s0, " ^ int i ^ "\n",
				src = [munchExp e1],
				dst = [r],
				jump = NONE}))
	  | munchExp (T.BINOP(oper, e1, e2)) =
	    result(fn r =>
		      emit(A.OPER{
				assem = getString(oper) ^ " `d0, `s0, `s1\n",
				src = [munchExp e1, munchExp e2],
				dst = [r],
				jump = NONE}))		  
	  | munchExp (T.NAME name) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "la `d0, " ^ S.name i ^ "\n",
				src = [],
				dst = [r],
				jump = NONE}))
	  | munchExp (T.CONST c) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "li `d0, " ^ int i ^ "\n",
				src = [],
				dst = [r],
				jump = NONE}))
	  | munchExp (T.TEMP temp) = temp
	  | munchExp _ = (ErrorMsg.error ~1 "Error while generating assembly"; Temp.newtemp()) 


	and munchArgs(index, args) =
	    if index >= List.length args
	    then []
	    else
		let val curArg = List.nth(args, index)
		in
		    if index >= 4
		    then
			(munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP F.SP, T.CONST(((index + 1) - 4) * 4))), curArg));
			 munchArgs(index + 1, args));
		    else
			(munchStm(T.MOVE(T.TEMP (List.nth(F.getArgRegs(), index)), curArg));
			 [munchExp(T.TEMP(List.nth(F.getArgRegs(), index)))] @ munchArgs(index + 1, args)
		end
    in munchStm stm;
       rev(!ilist)
    end
end

structure MipsGen : CODEGEN =
struct

structure T = Tree
structure A = Assem
structure S = Symbol
structure Frame = MipsFrame
		  
fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
    let val ilist = ref (nil: A.instr list)
	fun emit x = ilist := x :: !ilist
	fun result (gen) = let val t = Temp.newtemp() in gen t; t end
	fun int (x: int) =
	    if (x>=0) then Int.toString x
	    else "-" ^ Int.toString (~x)

	fun getBinop (T.PLUS) = "add"
	  | getBinop (T.MINUS) = "sub"
	  | getBinop (T.MUL) = "mul"
	  | getBinop (T.DIV) = "div"

	fun getRelop (T.EQ) = "beq"
	  | getRelop (T.NE) = "bne"
	  | getRelop (T.LT) = "blt"
	  | getRelop (T.GT) = "bgt"
	  | getRelop (T.LE) = "ble"
	  | getRelop (T.GE) = "bge"
				    
	fun munchStm (T.SEQ(a, b)) = (munchStm a; munchStm b)
					 
	  | munchStm (T.CJUMP(oper, e1, e2, tl, fl)) =
	    emit (A.OPER{assem = getRelop(oper) ^ " `s0, `s1, " ^ S.name(tl) ^ " \n",
			 src = [munchExp e1, munchExp e2],
			 dst = [],
			 jump = SOME([tl, fl])}) (* todo: fl, ?*)
	  | munchStm (T.JUMP(T.NAME lab, labs)) =
	    emit (A.OPER{assem = "j " ^ S.name(lab) ^ " \n",
			 src = [],
			 dst = [],
			 jump = SOME(labs)})
		 
	  (* sw *)
	  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, e1, T.CONST i)), e2)) =
	    emit (A.OPER{assem = "sw 's0, " ^ int i ^ "(`s1) \n",
			 src = [munchExp e2, munchExp e1],
			 dst = [], jump = NONE})
	  | munchStm (T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST i, e1)), e2)) =
	    emit (A.OPER{assem = "sw 's0, " ^ int i ^ "(`s1) \n",
			 src = [munchExp e2, munchExp e1],
			 dst = [], jump = NONE})
	  | munchStm (T.MOVE(T.MEM(e1), e2)) =
	    emit (A.OPER{assem = "sw `s0, 0(`s1) \n",
			 src = [munchExp e2, munchExp e1],
			 dst = [], jump = NONE})
	  | munchStm (T.MOVE(T.TEMP i, e2)) =
	    emit (A.MOVE{assem = "move `d0, `s0\n",
			 src = munchExp e2,
			 dst = i})
	  | munchStm (T.EXP e) = (munchExp e; ())
				     	  (* label *)
	  | munchStm (T.LABEL label) =
	    emit(A.LABEL{assem = S.name label ^ ":\n", lab=label})

	  | munchStm _ = (ErrorMsg.error ~1 "Error while generating assembly"; ()) 
			     
	and munchExp (T.CALL(T.NAME name, argList)) =
	    let val frameOffset = if List.length argList > 4
			      then (List.length(argList) - 4) * ~4
			      else 0
		val spMove = T.MOVE(T.TEMP Frame.SP, T.BINOP(T.PLUS, T.TEMP Frame.SP, T.CONST frameOffset))
		val spReturn = T.MOVE(T.TEMP Frame.SP, T.BINOP(T.PLUS, T.TEMP Frame.SP, T.CONST(~1 * frameOffset)))
	    in
		(munchStm spMove;
		 (emit (A.OPER{
			     assem = "jal " ^ S.name name ^ "\n",
			     src = munchArgs(0, argList),
			     dst = Frame.getCallerSaves() @ Frame.getReturnRegisters() @ [Frame.getReturnAddress()] @ Frame.getArgRegs(),
			     jump = NONE}));
		 munchStm spReturn;
		 Frame.V0)
					    
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
				assem = "addi `d0, `s0, " ^ int c ^ "\n",
				src = [munchExp e1],
				dst = [r],
				jump = NONE}))
	  | munchExp (T.BINOP(T.PLUS, T.CONST c, e1)) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "addi `d0, `s0, " ^ int c ^ "\n",
				src = [munchExp e1],
				dst = [r],
				jump = NONE}))
	  | munchExp (T.BINOP(oper, e1, e2)) =
	    result(fn r =>
		      emit(A.OPER{
				assem = getBinop(oper) ^ " `d0, `s0, `s1\n",
				src = [munchExp e1, munchExp e2],
				dst = [r],
				jump = NONE}))		  
	  | munchExp (T.NAME name) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "la `d0, " ^ S.name name ^ "\n",
				src = [],
				dst = [r],
				jump = NONE}))
	  | munchExp (T.CONST c) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "li `d0, " ^ int c ^ "\n",
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
			(munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.TEMP Frame.SP, T.CONST(((index + 1) - 4) * 4))), curArg));
			 munchArgs(index + 1, args))
		    else
			(munchStm(T.MOVE(T.TEMP (List.nth(Frame.getArgRegs(), index)), curArg));
			 [munchExp(T.TEMP(List.nth(Frame.getArgRegs(), index)))] @ munchArgs(index + 1, args) )
		end
    in munchStm stm;
       rev(!ilist)
    end
end

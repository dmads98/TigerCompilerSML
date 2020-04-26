structure MipsGen : CODEGEN =
struct

structure T = Tree
structure A = Assem
structure S = Symbol
structure Frame = MipsFrame
structure F = Frame
		  
fun codegen (frame) (stm: Tree.stm) : Assem.instr list =
    let val ilist = ref (nil: A.instr list)
	fun emit x = ilist := x :: !ilist
	fun result (gen) = let val t = Temp.newtemp() in gen t; t end
	fun int (x: int) =
	    if (x >= 0) then Int.toString x
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
	  | getRelop (T.UGE) = "bgeu"
	  | getRelop (T.UGT) = "bgtu"
	  | getRelop (T.ULE) = "bleu"
	  | getRelop (T.ULT) = "bltu"
				   
				    
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
		 
	  | munchStm (T.JUMP(exp1, labs)) =
	    emit (A.OPER{assem = "jr `j0\n",
			 src = [munchExp exp1],
			 dst = [],
			 jump = SOME(labs)})
	  (* sw *)
	  | munchStm (T.MOVE(T.TEMPPOS i, T.MEM(T.BINOP(T.PLUS, T.CONST(c), exp1)))) =
	    emit (A.OPER{assem = "lw `d0, " ^ int(c) ^ "(`s0)\n",
			 src = [munchExp exp1],
			 dst = [i],
			 jump = NONE})
		 
	  | munchStm (T.MOVE(T.TEMPPOS i, T.MEM(T.BINOP(T.PLUS, exp1, T.CONST(c))))) =
	    emit (A.OPER{assem = "lw `d0, " ^ int(c) ^ "(`s0)\n",
			 src = [munchExp exp1],
			 dst = [i],
			 jump = NONE})
	  | munchStm (T.MOVE(T.TEMPPOS i, T.MEM(T.BINOP(T.MINUS, exp1, T.CONST(c))))) =
	    emit (A.OPER{assem = "lw `d0, " ^ int(~c) ^ "(`s0)\n",
			 src = [munchExp exp1],
			 dst = [i],
			 jump = NONE})
	  | munchStm (T.MOVE(T.TEMPPOS i, T.NAME(n))) =
	    emit (A.OPER{assem = "la `d0, " ^ S.name(n) ^ "\n",
			 src = [],
			 dst = [i],
			 jump = NONE})
	  | munchStm (T.MOVE(T.TEMPPOS i, T.CONST(c))) =
	    emit (A.OPER{assem = "li `d0, " ^ int(c) ^ "\n",
			 src = [],
			 dst = [i],
			 jump = NONE})
	  | munchStm (T.MOVE(T.TEMPPOS i, exp1)) =
	    emit (A.MOVE{assem = "move `d0, `s0\n",
			 src = munchExp(exp1),
			 dst = i})

		 
	  | munchStm (T.MOVE(T.ESEQPOS(stm1, T.MEMPOS(exp1)), exp2)) =
	    (munchStm stm1;
	     emit (A.OPER{assem = "sw `s0, (`d0)\n",
			  src = [munchExp exp2],
			  dst = [munchExp exp1],
			  jump = NONE}))
	  | munchStm (T.MOVE(T.ESEQPOS(stm1, T.TEMPPOS(t)), exp1)) =
	    (munchStm stm1;
	     emit (A.MOVE{assem = "move `d0, `s0\n",
			  src = munchExp exp1,
			  dst = t}))
	  | munchStm (T.MOVE(T.ESEQPOS(stm1, expPos as T.ESEQPOS _), exp1)) =
	    (munchStm stm1;
	     munchStm (T.MOVE(expPos, exp1)))
		
	  | munchStm (T.MOVE(T.MEMPOS(T.BINOP(T.PLUS, T.CONST c, exp1)), exp2)) =
	    emit (A.OPER{assem = "sw `s0, " ^ int c ^ "(`s1)\n",
			 src = [munchExp exp2, munchExp exp1],
			 dst = [], jump = NONE})
	  | munchStm (T.MOVE(T.MEMPOS(T.BINOP(T.PLUS, exp1, T.CONST c)), exp2)) =
	    emit (A.OPER{assem = "sw `s0, " ^ int c ^ "(`s1)\n",
			 src = [munchExp exp2, munchExp exp1],
			 dst = [], jump = NONE})
	  | munchStm (T.MOVE(T.MEMPOS(T.BINOP(T.MINUS, exp1, T.CONST c)), exp2)) =
	    emit (A.OPER{assem = "sw `s0, " ^ int(~c) ^ "(`s1)\n",
			 src = [munchExp exp2, munchExp exp1],
			 dst = [], jump = NONE})
	  | munchStm (T.MOVE(T.MEMPOS(e1), e2)) =
	    emit (A.OPER{assem = "sw `s0, 0(`s1)\n",
			 src = [munchExp e2, munchExp e1],
			 dst = [], jump = NONE})
	 
	  | munchStm (T.EXP e) = (munchExp e; ())
				     	  (* label *)
	  | munchStm (T.LABEL label) =
	    emit(A.LABEL{assem = S.name label ^ ":\n", lab=label}) 
			     
	and munchExp (T.CALL(T.NAME name, argList)) =
	    let val liveRegs = F.RA ::
			       F.RV ::
			       F.V1 ::
			       (F.getRegTemps(F.callersaves)) @ (F.getRegTemps(F.argregs))
	    in 
		(emit (A.OPER{
			    assem = "jal " ^ S.name name ^ "\n",
			    src = munchArgs(0, 16, argList),
			    dst = liveRegs,
			    jump = NONE});
		 F.RV)
		    
	    end
	  | munchExp (T.CALL(_, _)) = (ErrorMsg.error ~1 "CALL does not have same NAME as exp";
				       Temp.newtemp())
	  | munchExp (T.MEM(T.BINOP(T.PLUS, e1, T.CONST i))) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "lw `d0, " ^ int i ^ "(`s0)\n",
				src = [munchExp e1],
				dst = [r],
				jump = NONE
		  }))
	  | munchExp (T.MEM(T.BINOP(T.PLUS, T.CONST i, e1))) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "lw `d0, " ^ int i ^ "(`s0)\n",
				src = [munchExp e1],
				dst = [r],
				jump = NONE
		  }))
	  | munchExp (T.MEM(T.BINOP(T.MINUS, e1, T.CONST i))) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "lw `d0, " ^ int(~i) ^ "(`s0)\n",
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
	  | munchExp (T.BINOP(T.MINUS, e1, T.CONST c)) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "addi `d0, `s0, " ^ int(~c) ^ "\n",
				src = [munchExp e1],
				dst = [r],
				jump = NONE}))
	  | munchExp (T.BINOP(T.LSHIFT, e1, T.CONST c)) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "sll `d0, `s0, " ^ int(c) ^ "\n",
				src = [munchExp e1],
				dst = [r],
				jump = NONE}))
	  | munchExp (T.BINOP(T.LSHIFT, exp1, exp2)) = (ErrorMsg.error ~1 "could not create sll instruction";
							Temp.newtemp())
							   
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
	  | munchExp (T.ESEQ(stm1, exp1)) = (munchStm stm1;
					     munchExp exp1)
	  | munchExp (T.CONST 0) = F.R0
	  | munchExp (T.CONST c) =
	    result(fn r =>
		      emit(A.OPER{
				assem = "li `d0, " ^ int c ^ "\n",
				src = [],
				dst = [r],
				jump = NONE}))
	  | munchExp (T.TEMP temp) = temp
			     
			     
	and munchArgs (_, _, []) = []
	  | munchArgs (index, offset, arg::ls) =
	    let val curTemp = if index >= 4
			     then Temp.newtemp()
			     else List.nth(F.getRegTemps(F.argregs), index)
	    in
		if index >= 4
		then (munchStm(T.MOVE(T.MEMPOS(T.BINOP(T.PLUS, T.TEMP(F.SP), T.CONST offset)), arg));
		      munchArgs(index + 1, offset + 4, ls))
		else (munchStm(T.MOVE(T.TEMPPOS(curTemp), arg));
		      curTemp::munchArgs(index + 1, offset, ls))
	    end
    in munchStm stm;
       rev(!ilist)
    end
end

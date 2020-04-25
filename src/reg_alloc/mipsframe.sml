structure MipsFrame : FRAME =
struct

datatype access = InFrame of int (* InFrame(X) = mem location at offset X from frame pointer *)
		| InReg of Temp.temp (* InReg(t84) = held in register t84 *)
(* InFrame and InReg are not visible outside module. Interface functions from chap 7 *)

val R0 = Temp.newtemp()
val AT = Temp.newtemp()
		     
val RV = Temp.newtemp()
val V1 = Temp.newtemp()
		     
val A0 = Temp.newtemp()
val A1 = Temp.newtemp()
val A2 = Temp.newtemp()
val A3 = Temp.newtemp()
		     
val T0 = Temp.newtemp()
val T1 = Temp.newtemp()
val T2 = Temp.newtemp()
val T3 = Temp.newtemp()
val T4 = Temp.newtemp()
val T5 = Temp.newtemp()
val T6 = Temp.newtemp()
val T7 = Temp.newtemp()
val T8 = Temp.newtemp()
val T9 = Temp.newtemp()
		     
val S0 = Temp.newtemp()
val S1 = Temp.newtemp()
val S2 = Temp.newtemp()
val S3 = Temp.newtemp()
val S4 = Temp.newtemp()
val S5 = Temp.newtemp()
val S6 = Temp.newtemp()
val S7 = Temp.newtemp()
		     
val K0 = Temp.newtemp()
val K1 = Temp.newtemp()
		     
val GP = Temp.newtemp()
val SP = Temp.newtemp()
val FP = Temp.newtemp()
val RA = Temp.newtemp()
		     
val wordSize = 4
val numArgsInRegs = 4

type frame = {name: Temp.label, formals: access list, numLocalsAlloc: int ref, offset : int ref}
datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string

(* MIPS format string *)
(* fun string (label, s) : string = ".data\n" ^ Symbol.name(label) ^ ":\n.word " ^ Int.toString(String.size(s)) ^ "\n.ascii \"" ^ s ^ "\"\n" ^ ".text\n" *)

fun string (label, string) : string = Symbol.name(label) ^ ": .asciiz \"" ^ string ^ "\"\n";


(* from translate *)
fun seq [s] = s
  | seq [s1, s2] = Tree.SEQ(s1, s2)
  | seq (a::l) = Tree.SEQ(a, seq l)
  | seq [] = (Tree.EXP(Tree.CONST(0)))
						
val argregs = [(A0, "$a0"), (A1, "$a1"), (A2, "$a2"), (A3, "$a3")]
val specialregs = [(R0, "$zero"), (AT, "$at"), (RV, "$v0"), (V1, "$v1"), (K0, "$k0"), (K1, "$k1"), (GP, "$gp"), (SP, "$sp"), (FP, "$fp"), (RA, "$ra")]
val calleesaves = [(S0, "$s0"), (S1, "$s1"), (S2, "$s2"), (S3, "$s3"), (S4, "$s4"), (S5, "$s5"), (S6, "$s6"), (S7, "$s7")]
val callersaves = [(T0, "$t0"), (T1, "$t1"), (T2, "$t2"), (T3, "$t3"), (T4, "$t4"), (T5, "$t5"), (T6, "$t6"), (T7, "$t7"), (T8, "$t8"), (T9, "$t9")]

fun getRegTemps (ls) = map (fn (reg, name) => reg) ls
			       
val tempMap = let fun addToMap ((reg, name), curMap) = Temp.Table.enter(curMap, reg, name)
	      in
		  foldl addToMap Temp.Table.empty (specialregs @ argregs @ calleesaves @ callersaves)
	      end

fun getRegName reg = case Temp.Table.look(tempMap, reg) of SOME(s) => s (* todo: may fix*)
							 | NONE => Temp.makestring(reg)

			   
fun name {name, formals, numLocalsAlloc, offset} = name
fun formals {name, formals, numLocalsAlloc, offset} = formals
fun allocLocal ({name, formals, numLocalsAlloc, offset}) (esc) =
		(numLocalsAlloc := !numLocalsAlloc + 1;
		 case esc of true => (offset := !offset - wordSize;
				      InFrame(!offset))
			   | false => InReg(Temp.newtemp()))
										   

fun newFrame {name, formals} =
    let fun formalsAlloc ([], list, index, offset) = list
	  | formalsAlloc (f::ls, list, index, offset) =
	    (case f of true => (InFrame offset)::formalsAlloc(ls, list, index + 1, offset + wordSize)
		     | false => (InReg(Temp.newtemp()))::formalsAlloc(ls, list, index + 1, offset + wordSize)) 
    in
	{name = name, formals = formalsAlloc(formals, [], 0, 0), numLocalsAlloc = ref 0, offset = ref ~44}
    end

fun exp (InReg(k), fP) = Tree.TEMP(k)
  | exp (InFrame(k), fP) = Tree.MEM(Tree.BINOP(Tree.PLUS, fP, Tree.CONST(k)))

fun externalCall (funcName, expList) = Tree.CALL(Tree.NAME(Temp.namedlabel(funcName)), expList)

fun int (x: int) =
    if (x>=0) then Int.toString x
    else "-" ^ Int.toString (~x)

fun convertToPos (Tree.TEMP t) = Tree.TEMPPOS t
  | convertToPos (Tree.MEM e) = Tree.MEMPOS e
  | convertToPos (Tree.ESEQ (s, e as Tree.MEM(_))) = Tree.ESEQPOS(s, convertToPos(e))
  | convertToPos (Tree.ESEQ (s, t as Tree.TEMP(_))) = Tree.ESEQPOS(s, convertToPos(t))
  | convertToPos _ = (ErrorMsg.error 0 "error in converting exp to pos";
		      Tree.TEMPPOS(Temp.newtemp()))
					       
fun procEntryExit1 (fr : frame, body : Tree.stm) =
    let val tempList = getRegTemps argregs
	fun mvArgReg (offset, []) = []
	  | mvArgReg (offset, acc::ls) =
	    if offset >= 4
	    then case acc of InReg t =>
			     Tree.MOVE(convertToPos(exp(acc, Tree.TEMP FP)), Tree.TEMP(t)) :: mvArgReg(offset + 1, ls)
			  | InFrame _ => mvArgReg(offset + 1, ls)
	    else Tree.MOVE(convertToPos(exp(acc, Tree.TEMP(FP))),
			   Tree.TEMP(List.nth(tempList, offset))) :: mvArgReg(offset + 1, ls)
    in
	seq(mvArgReg(0, (formals fr)) @ [body])
    end
	
fun procEntryExit2 (frame : frame, body) =
    body @ [Assem.OPER{assem = "",
		       src = (getRegTemps specialregs),
		       dst = [],
		       jump = SOME ([])}]

(*
fun procEntryExit3 ({name, formals, numLocalsAlloc} : frame, label::body, spillList) =
    let val retInstr = Assem.OPER{assem = "jr `s0\n\n",
				  src = [RA],
				  dst = [],
				  jump = SOME([])}
	val saveRAInstr = Assem.OPER{assem = "sw `s0, " ^ int(!numLocalsAlloc * ~4) ^ "(`s1) \n",
				  src = [RA, FP],
				  dst = [],
				  jump = NONE}
	val loadRAInstr = Assem.OPER{assem = "lw `d0, " ^ int(!numLocalsAlloc * ~4) ^ "(`s0) \n",
				  src = [FP],
				  dst = [RA],
				  jump = NONE}
	val count = ref 0
	val storeSpillInstrs = map (fn spill => (count := !count + 1;
						 Assem.OPER{assem = "sw `s0, " ^ int((!numLocalsAlloc + !count + 1) * ~4) ^ "(`s1) \n",
							    src = [spill, FP],
							    dst = [],
							    jump = NONE})) spillList
	val count = ref 0
	val loadSpillInstrs = map (fn spill => (count := !count + 1;
						 Assem.OPER{assem = "lw `d0, " ^ int((!numLocalsAlloc + !count + 1) * ~4) ^ "(`s0) \n",
							    src = [FP],
							    dst = [spill],
							    jump = NONE})) spillList
	val fpPos = (!numLocalsAlloc + 1) * ~4
	val sizeOfStack = (!numLocalsAlloc + List.length(spillList) + 2) * ~4
	val makeStackSpace = [Assem.OPER{assem = "sw `s0, " ^ int(fpPos) ^ "(`s1) \n",
					 src=[FP, SP],
					 dst = [],
					 jump = NONE},
			      Assem.MOVE{assem = "move `d0, `s0\n",
					 src = SP,
					 dst = FP},
			      Assem.OPER{assem = "addi `d0, `s0, " ^ int(sizeOfStack) ^ "\n",
					 src = [SP],
					 dst = [SP],
					 jump = NONE}]
	val restoreStack = [Assem.OPER{assem = "addi `d0, `s0, " ^ int(sizeOfStack * ~1) ^ "\n",
					 src=[SP],
					 dst = [SP],
					 jump = NONE},
			      Assem.OPER{assem = "lw `d0, " ^ int(fpPos) ^ "(`s0)\n",
					 src = [FP],
					 dst = [FP],
					 jump = NONE}]
    in
	{prolog = "PROCEDURE " ^ Symbol.name(name) ^ "\n",
	 body = [label] @ makeStackSpace @ [saveRAInstr] @ storeSpillInstrs @ body @ [loadRAInstr] @ loadSpillInstrs @ restoreStack @ [retInstr],
	 epilog = "END " ^ Symbol.name(name) ^ "\n"}
    end*)
					       
end

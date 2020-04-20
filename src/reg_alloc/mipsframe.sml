structure MipsFrame : FRAME =
struct

datatype access = InFrame of int (* InFrame(X) = mem location at offset X from frame pointer *)
		| InReg of Temp.temp (* InReg(t84) = held in register t84 *)
(* InFrame and InReg are not visible outside module. Interface functions from chap 7 *)

val ZERO = Temp.newtemp()
val AT = Temp.newtemp()
val V0 = Temp.newtemp()
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

type frame = {name: Temp.label, formals: access list, numLocalsAlloc: int ref}
datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string

(* MIPS format string *)
fun string (label, s) : string = S.name label ^ ": .asciiz \"" ^ s ^ "\"\n"

val argregs = [(A0, "$a0"), (A1, "$a1"), (A2, "$a2"), (A3, "$a3")]
val specialregs = [(ZERO, "$r0"), (AT, "$at"), (V0, "$v0"), (V1, "$v1"), (K0, "$k0"), (K1, "$k1"), (GP, "$gp"), (SP, "$sp"), (FP, "$fp"), (RA, "$ra")]
val calleesaves = [(S0, "$s0"), (S1, "$s1"), (S2, "$s2"), (S3, "$s3"), (S4, "$s4"), (S5, "$s5"), (S6, "$s6"), (S7, "$s7")]
val callersaves = [(T0, "$t0"), (T1, "$t1"), (T2, "$t2"), (T3, "$t3"), (T4, "$t4"), (T5, "$t5"), (T6, "$t6"), (T7, "$t7"), (T8, "$t8"), (T9, "$t9")]

fun getCalleeSaves() = map (fn (reg, name) => reg) calleesaves
fun getCallerSaves() = map (fn (reg, name) => reg) callersaves
fun getReturnAddress() = RA
fun getReturnRegisters() = [V0, V1]
fun getArgRegs() = map (fn (reg, name) => reg) argregs

val tempMap = let fun addToMap ((reg, name), curMap) = Temp.Table.enter(curMap, reg, name)
	      in
		  foldl addToMap Temp.Table.empty (specialregs @ argregs @ calleesaves @ callersaves)
	      end

fun getRegName reg = case Temp.Table.look(tempMap, reg) of SOME(s) => s (* todo: may fix*)
							 | NONE => Temp.makestring(reg)

			   
fun name {name, formals, numLocalsAlloc} = name
fun formals {name, formals, numLocalsAlloc} = formals
fun allocLocal ({name, formals, numLocalsAlloc} : frame) (true) = (numLocalsAlloc := !numLocalsAlloc + 1;
										     InFrame(~1 * (!numLocalsAlloc - 1) * wordSize))
  | allocLocal ({name, formals, numLocalsAlloc} : frame) (false) = InReg(Temp.newtemp())

fun newFrame {name, formals} =
    let val numRegsInUse = ref 0
	val numStackFormals = ref 0
	fun checkBools true = (numStackFormals := !numStackFormals + 1;
			       InFrame(~1 * (!numStackFormals - 1) * wordSize))
	  | checkBools false = if !numRegsInUse >= numArgsInRegs
			       then checkBools true
			       else (numRegsInUse := !numRegsInUse + 1;
				     InReg(Temp.newtemp()))
    in
	{name = name, formals = (map checkBools formals), numLocalsAlloc = numStackFormals}
    end

fun exp (InReg(k)) (fP) = Tree.TEMP(k)
  | exp (InFrame(k)) (fP) = Tree.MEM(Tree.BINOP(Tree.PLUS, fP, Tree.CONST(k)))

fun externalCall(funcName, expList) = Tree.CALL(Tree.NAME(Temp.namedlabel(funcName)), expList)

(*fun procEntryExit1 (frame, body) =
    let val
    in
	[Tree.LABEL(name frame)] @ 
    end
*)
fun procEntryExit2 (frame, body) = body @ [Assem.OPER{assem = "",
						      src = (map (fn (reg, name) => reg) calleesaves) @ (map (fn (reg, name) => reg) specialregs),
						      dst = [],
						      jump = SOME ([])}]

fun procEntryExit3 ({name, formals, numLocalsAlloc}, body) = {prolog = "PROCEDURE " ^ Symbol.name(name) ^ "\n",
							      body = body,
							      epilog = "END " ^ Symbol.name(name) ^ "\n"}
					       
end

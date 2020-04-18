structure MipsFrame : FRAME =
struct

datatype access = InFrame of int (* InFrame(X) = mem location at offset X from frame pointer *)
		| InReg of Temp.temp (* InReg(t84) = held in register t84 *)
(* InFrame and InReg are not visible outside module. Interface functions from chap 7 *)

val FP = Temp.newtemp()
val RV = Temp.newtemp()
val wordSize = 4
val numArgsInRegs = 4

type frame = {name: Temp.label, formals: access list, numLocalsAlloc: int ref}
datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string
					       

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

end

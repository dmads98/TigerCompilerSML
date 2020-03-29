structure MipsFrame : FRAME =
struct

datatype access = InFrame of int (* InFrame(X) = mem location at offset X from frame pointer *)
		| InReg of Temp.temp (* InReg(t84) = held in register t84 *)
(* InFrame and InReg are not visible outside module. Interface functions from chap 7 *)



end;

structure Frame : FRAME = MipsFrame;

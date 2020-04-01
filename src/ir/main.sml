(*  *)
structure Main =
struct

structure TR = Translate

fun main filename =
    let
	val t = ErrorMsg.reset();
	val tree = Parse.parse filename; (* Absyn.exp *)
	(* transProg: Absyn.exp -> unit *)
    in
	(* Printtree.printtree(TextIO.stdOut, TR.unNx (Semant.transProg tree)) *)
	Semant.transProg tree
    end
end

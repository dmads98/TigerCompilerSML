(*  *)
structure Main =
struct

fun main filename =
    let
	val tree = Parse.parse filename; (* Absyn.exp *)
	(* transProg: Absyn.exp -> unit *)
    in
	Semant.transProg tree
    end
end

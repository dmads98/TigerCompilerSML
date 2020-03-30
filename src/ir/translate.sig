signature TRANSLATE =
sig
    type level
    type access (* Different from Frame.access *)
    type exp
    type frag

    val reset : unit -> unit
    val getResult : unit -> frag list

    val procEntryExit : {level: level, body: exp} -> unit (* Remember proc fragments *)

    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals : level -> access list
    val allocLocal : alloc -> bool -> access

    val simpleVar : access * level -> exp (* pg 154 *)
    val transIF : (exp * exp * exp option) -> exp
    val transBREAK : Temp.label -> exp
    val transWHILE : (exp * exp * Temp.label) -> exp
    val transDO_WHILE : (exp * exp * Temp.label) -> exp
    val transBINOP : (exp * Absyn.oper * exp) -> exp
    val transRELOP : (exp * Absyn.oper * exp) -> exp
    val transASSIGN : (exp * exp) -> exp
    val transLet : (exp list * exp) -> exp
    val transInt : int -> exp
    val transSTRING : string -> exp
    val transARRAY : (exp * exp) -> exp
    val transRECORD : exp list -> exp
    val transSEQ : exp list -> exp
    val transNIL : exp

end

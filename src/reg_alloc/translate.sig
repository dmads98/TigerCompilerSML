signature TRANSLATE =
sig
    type level
    type access (* Different from Frame.access *)
    type frag
    type exp

    val reset : unit -> unit
    val getResult : unit -> frag list

    val procEntryExit : {level: level, body: exp} -> unit (* Remember proc fragments *)

    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals : level -> access list
    val allocLocal : level -> bool -> access

    val simpleVar : access * level -> exp (* pg 154 *)
    val fieldVar : (exp * int) -> exp
    val subscriptVar : (exp * exp) -> exp

    val transCONST : (int -> exp)
    val transIFELSE : (exp * exp * exp) -> exp
    val transIFTHEN : (exp * exp) -> exp
    val transBREAK : Temp.label -> exp
    val transWHILE : (exp * exp * Temp.label) -> exp
    val transFOR : (exp * bool ref * exp * exp * exp * Temp.label) -> exp
    (* val transDO_WHILE : (exp * exp * Temp.label) -> exp *)
    val transBINOP : (exp * Absyn.oper * exp) -> exp
    val transRELOP : (exp * Absyn.oper * exp * Types.ty) -> exp
    val transASSIGN : (exp * exp) -> exp
    val transLET : (exp list * exp) -> exp
    val transINT : int -> exp
    val transSTRING : string -> exp
    val transARRAY : (exp * exp) -> exp
    val transRECORD : exp list -> exp
    val transSEQ : exp list -> exp
    val transNIL : exp

    val transCALL : level * level * Temp.label * exp list -> exp
end

structure Translate =
struct

structure T = Tree
structure F = Frame

datatype exp = Ex of T.exp
	     | Nx of T.stm
	     | Cx of Temp.label * Temp.label -> Temp.stm
						    
(* frag list ref local *)
						    
val procEntryExit : {level: level, body: exp} -> unit (* Remember proc fragments *)

structure Frame : FRAME = MipsFrame
val getResult : unit -> Frame.frag list
val newLevel (* Function *)

(* exp -> T.exp *)
fun unEx (Ex e) = e
  | unEx (Cx genstm) =
    let val r = Temp.newtemp() (* register to store true/false result *)
	val t  = Temp.newlabel() and f = Temp.newlabel () (* Jump Labels: true and false *)
    in T.ESEQ(seq[T.MOVE(T.TEMP r, T.const 1),
		  genstm(t, f),
		  T.LABEL f,
		  T.MOVE(T.Temp r, T.CONST 0),
		  T.LABEL t],
	      T.TEMP r)
    end
  | unEx (Nx s) = T.ESEQ(s, T.CONST 0)

(* exp -> T.stm *)
fun unNx (Ex e) = T.EXP e (* Constructor T.EXP converts expression to a stm *)
  | unNx (Cx genstm) =
    let val x = Temp.newlabel ()
    in SEQ(genstm(x, x), T.LABEL x)
    end
  | unNx (Nx s) = s

(* exp -> (Temp.label * Temp.label -> T.stm) *)
fun unCx (Ex e) =
    case e of
	T.CONST 0 => (fn(t, f) => T.JUMP(T.NAME f, [f])) (* always false *)
      | T.CONST 1 => (fn(t, f) => T.JUMP(T.NAME t, [t])) (* always true *)
      | exp => (fn(t, f) => T.CJUMP(T.EQ, exp, T.CONST 0, f, t)) (* If exp=0, jump to false *)
  | unCx (Cx genstm) = genstm (* Return the genstm function *)
  | unCx (Nx genstm) = raise ErrorMsg.Error
    
end;
(* Assume every variable escapes, keep in local frame and don't bother with findEscape *)

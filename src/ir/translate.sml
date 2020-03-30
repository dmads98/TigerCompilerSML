structure Translate =
struct

structure Frame : FRAME = MipsFrame

structure T = Tree
structure F = Frame
structure A = Absyn

datatype exp = Ex of T.exp
	     | Nx of T.stm
	     | Cx of Temp.label * Temp.label -> Temp.stm

type frag = F.frag
val fragments : frag list ref = ref nil (* frag list ref local *)
fun reset () = fragments := nil
fun getResult (): (F.frag list) = !fragments


						    
val procEntryExit : {level: level, body: exp} -> unit (* Remember proc fragments *)

val newLevel (* Function *)

(* exp -> T.exp *)
fun unEx (Ex e) = e
  | unEx (Cx genstm) =
    let val res = Temp.newtemp() (* register to store true/false result *)
	val t  = Temp.newlabel() and f = Temp.newlabel () (* Jump Labels: true and false *)
    in T.ESEQ(seq[T.MOVE(T.TEMP res, T.const 1),
		  genstm(t, f),
		  T.LABEL f,
		  T.MOVE(T.Temp res, T.CONST 0),
		  T.LABEL t],
	      T.TEMP res)
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
  | unCx (Nx genstm) = raise ErrorMsg.Error;


(********** IF WHILE FOR  **********)

(* Return an exp *)
fun transIF (cond, thenexp, elseexp) =
    let val cond' = unCx(cond)
	val then' = unEx(thenexp)
	val else' = unEx(elseexp)
	val res = Temp.newtemp ()
	val lt = Temp.newlabel ()
	and lf = Temp.newlabel ()
	and le = Temp.newlabel ()
    in Ex(T.ESEQ(seq[cond'(lt, lf),
		     T.LABEL lt,
		     T.MOVE(T.TEMP res, then'),
		     T.JUMP(T.NAME le, [le]),
		     T.LABEL lf,
		     T.MOVE(T.TEMP res, else'),
		     T.LABEL le
		    ],
		 T.TEMP res))
    end;

fun transBREAK (label) : exp = Nx(T.JUMP(T.NAME label, [label]));

(* Return Nx (T.stm) *)
fun transWHILE (cond, body, lend) =
    let val cond' = unCx cond
	val body' = unNx body
	val ltest = Temp.newlabel ()
	and lstart = Temp.newlabel ()
    in Nx(seq[T.JUMP(T.NAME ltest, [ltest]),
	      T.LABEL lstart,
	      body',
	      T.LABEL ltest,
	      cond' (lstart, lend),
	      T.LABEL lend
	 )]
    end;

(* do-while same as above without first jump to test *)
fun transDO_WHILE (cond, body, lend) =
    let val cond' = unCx cond
	val body' = unNx body
	val ltest = Temp.newlabel ()
	and lstart = Temp.newlabel ()
    in Nx(seq[T.LABEL lstart,
	      body',
	      T.LABEL ltest,
	      cond' (lstart, lend),
	      T.LABEL lend
	 ])
    end;

(********** FUNCTIONS: nested->non-nested: explict frame management  **********)


(********** LET, FUN: Nested Lexical Scoping -> Global Scope **********)
fun transLet () = ;

(********** DATA STRUCTURES: int, string, record, array  **********)
fun transINT (num : int) = Ex(T.CONST num);

(* T.STRING = (Temp.label * string) *)
fun transSTRING (str : string) =
    let fun isString (F.PROC _) = false
	  | isString (F.STRING (l, s)) = str = s
	val res = List.find(isString) (!fragments)
    in case res of
	   SOME (F.STRING(l, s)) => Ex(T.NAME l) (* return label if found *)
	 | NONE => let val nl = Temp.newlabel () (* else, insert into frags as new string *)
		   in (fragments := F.STRING (nl, str) :: (!fragments);
		       Ex(T.NAME nl)) (* and return the new label *)
		   end
    end;

fun transBINOP (left, oper, right) =
    let val left' = unEx left
	val right' = unEx right
	val oper' =
	    case oper of
		A.PlusOp => T.PLUS
	      | A.MinusOp => T.MINUS
	      | A.TimesOp => T.MUL
	      | A.DivideOp => T.DIV
    in Ex(T.binop(oper', left', right'))
    end;

fun transRELOP (left, oper, right) =
    let val left' = unEx left
	val right' = unEx right
	val oper' =
	    case oper of
		A.EqOp => T.EQ
	      | A.NeqOp => T.NE
	      | A.LtOp => T.LT
	      | A.LeOp => T.LE
	      | A.GtOp => T.GT
	      | A.GeOp => T.GE
    in Cx(fn (t, f) => T.CJUMP(oper', left', right', t, f) )
    end;

fun transASSIGN (lhs, rhs) =
    let val lhs' = unEx lhs
	val rhs' = unEx rhs
    in Nx(T.MOVE(lhs', rhs'))
    end;


	    

end;
(* Assume every variable escapes, keep in local frame and don't bother with findEscape *)

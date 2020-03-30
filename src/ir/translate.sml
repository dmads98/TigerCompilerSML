structure Translate : TRANSLATE =
struct

structure Frame : FRAME = MipsFrame

structure T = Tree
structure F = Frame
structure A = Absyn

datatype exp = Ex of T.exp
	     | Nx of T.stm
	     | Cx of Temp.label * Temp.label -> Temp.stm

(***** Proc and String Fragments *****)
type frag = F.frag
val fragments : frag list ref = ref nil (* frag list ref local *)
fun reset () = fragments := nil
fun getResult (): (F.frag list) = !fragments

(***** Levels and Access *****)
datatype level = Top
	       | Level of {parent: level, frame: F.frame} * unit ref
type access = level * F.access
val outermost = Top
fun newLevel {parent, name, formals} =
    Level ({parent=parent, frame = F.newFrame({name=name, formals=true::formals})}, ref ());
fun formals (Top) = nil
  | formals (level as Level({parent, frame}, _)) =
    let val res_formals = tl (F.formals frame)
    in (map (fn x => (level, x)) res_formals)
    end; (* Test if correct *)
fun allocLevel () = ();

(***** IR translation *****)
fun seq [s] = s
  | seq [s1, s2] = T.SEQ(s1, s2)
  | seq (a::l) = T.SEQ(a, seq l);

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
  | unNx (Nx s) = s;

(* exp -> (Temp.label * Temp.label -> T.stm) pg 154 *)
fun unCx (Cx genstm) = genstm (* Return the genstm function *)
    | unCx (Nx _) = raise ErrorMsg.Error
    | unCx (Ex e) =
      case e of
	  T.CONST 0 => (fn(t, f) => T.JUMP(T.NAME f, [f])) (* always false *)
	| T.CONST 1 => (fn(t, f) => T.JUMP(T.NAME t, [t])) (* always true *)
	| exp => (fn(t, f) => T.CJUMP(T.EQ, exp, T.CONST 0, f, t)); (* If exp=0, jump to false *)


(********** IF WHILE FOR  **********)

(* Return an exp *)
fun transIF (cond, thenexp, elseexp) = (* allow for missing else? *)
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
	      T.LABEL lend])
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

(********** FUNCTIONS: nested->non-nested: explict frame management  **********)
fun procEntryExit (Level({rame, ...}, _), body) =
    let val body' = unEx body
	val body'' = F.procEntryExit1 (frame, T.MOVE(T.TEMP F.RV, body'))
    in fragments := F.PROC{frame=frame, body=body''}
    end;

fun transCALL () = ();

(********** LET, FUN: Nested Lexical Scoping -> Global Scope **********)
fun transLet (decs, body) = (* check if this works *)
    let val num_decs = List.length decs
	val body' = unEx body
	val decs' = map unEx decs
    in case num_decs of
	   0 => body
	 | _ => Ex(T.ESEQ(seq decs', body'))
    end;

(********** DATA STRUCTURES: int, string, record, array  **********)
val transNIL = Ex(T.CONST 0);

fun memInc (e1, e2) = T.MEM(T.BINOP(T.PLUS, e1, e2)); (* Increment memory/FP(T.TEMP + T.CONST) *)

(***** Var: SimpleVar, FieldVar, SubscriptVar *****)
(* pg 156: lf: level of use, lg: level of definition *)
fun simpleVar (access, lf) =
    let val (Level(_, ref_g), access_g) = access
	fun follow (Level({parent, frame}, cur_ref), cur_acc) =
	    if ref_g = cur_ref
	    then F.exp(ref_g)(cur_acc)
	    else let val next_link = hd (F.formals frame)
		 in follow (parent, F.exp(next_link)(cur_acc))
		 end
    in Ex(follow(lf, T.TEMP(F.FP)))
    end;

fun fieldVar (base, id, fieldList) =
    let fun findInd (ind, a::l) =
	    if id = a then ind else findInd(ind+1, l)
	val base' = unEx base
	val index = findInd(0, fieldList)
	val inc = T.BINOP(T.MUL, T.CONST(index), T.CONST(F.wordSize))
    in Ex(memInc(base', inc))
    end;

fun subscriptVar (base, offset) =
    let val base' = unEx base
	val offset' = unEx offset
	val inc = T.BINOP(T.MUl, offset', T.CONST(F.wordSize))
    in Ex(memInc(base', inc))
    end;

fun transINT (num : int) = Ex(T.CONST num);

(* T.STRING = (Temp.label * string) pg 163, 169 *)
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

fun transARRAY (size, init) =
    let val size' = unEx size
	val init' = unEx init
    in Ex(F.externalCall("initArray", [size', init']))
    end;

(* pg 164, 288 *)
fun transRECORD (fieldList) =
    let val r = Temp.newtemp ()
	val alloc = T.MOVE(T.TEMP r,
			   F.externalCall("allocRecord",
					  [T.CONST((length fieldList) * F.wordSize)])
			  )
	fun iter (nil, ind) = nil
	  | iter (a::l, ind) = T.MOVE(memInc(T.TEMP r, T.CONST (ind * F.wordsize)),
				      (unEx a)::(iter (l, ind+1)))
    in Ex(T.ESEQ(seq(alloc::iter(fieldList, 0)), T.TEMP r))
    end;

fun transSEQ [] = Nx(T.EXP(T.CONST 0))
  | transSEQ [e] = e
  | transSEQ exps =
    let val first = List.take(exps, length(exps)-1)
	val first' = seq(map unNx first)
	val last = List.last exps
	val last' = unEx last
    in case last of
	   Nx s => Nx(T.SEQ(first', s))
	 | _ => T.ESEQ(first', last')
    end;

end;
(* Assume every variable escapes, keep in local frame and don't bother with findEscape *)

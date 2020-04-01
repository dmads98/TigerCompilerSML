structure Translate : TRANSLATE =
struct

structure Frame : FRAME = MipsFrame

structure T = Tree
structure F = Frame
structure A = Absyn
structure Ty = Types

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
  | seq (a::l) = T.SEQ(a, seq l)
  | seq [] = (T.EXP(T.CONST(0)))
		 
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
fun transIFELSE (cond, thenexp, elseexp) =
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
    end

fun transIFTHEN (cond, thenexp) =
    let val cond' = unCx(cond)
	val then' = unEx(thenexp)
	val lt = Temp.newlabel ()
	and le = Temp.newlabel ()
    in Nx(seq[cond'(lt, le),
		     T.LABEL lt,
		     T.EXP(then'),
		     T.LABEL le
	     ])
    end


fun transBREAK (SOME(label)) = Nx(T.JUMP(T.NAME label, [label]))
  | transBREAK (NONE) = Nx(T.EXP(T.CONST 0)) 

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
    end

fun transFOR (lo, hi, body, lend) =
    let val lo' = unEx lo
	val hi' = unEx hi
	val body' = unNx body
	val loopVar = Temp.newtemp()
	val hiReg = Temp.newtemp()
	val lab1 = Temp.newlabel()
	val lab2 = Temp.newlabel()
    in
	Nx(seq[T.MOVE(T.TEMP(loopVar), lo'),
	       T.MOVE(T.TEMP(hiReg), hi'),
	       T.CJUMP(T.LE, T.TEMP(loopVar), T.TEMP(hiReg), L2, lend),
	       T.LABEL(lab1),
	       T.MOVE(T.TEMP(loopVar), T.BINOP(T.PLUS, T.TEMP(loopVar), T.CONST(1))),
	       T.LABEL(lab2),
	       body',
	       T.CJUMP(T.LT, T.TEMP(loopVar), T.TEMP(hiReg), L1, lend),
	       T.LABEL(lend)
	  ])
    end
	
(*
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
*)
fun transBINOP (left, oper, right) =
    let val left' = unEx left
	val right' = unEx right
	val oper' =
	    case oper of
		A.PlusOp => T.PLUS
	      | A.MinusOp => T.MINUS
	      | A.TimesOp => T.MUL
	      | A.DivideOp => T.DIV
    in Ex(T.BINOP(oper', left', right'))
    end;

fun transRELOP (left, oper, right, ty) =
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
    in
	let fun helper (T.EQ, exp1, exp2, Ty.STRING) = Ex(F.externalCall("stringEqual", [exp1, exp2]))
	      | helper (T.NE, exp1, exp2, Ty.STRING) = Ex(F.externalCall("stringNE", [exp1, exp2]))
 	      | helper (T.LE, exp1, exp2, Ty.STRING) = Ex(F.externalCall("stringLE", [exp1, exp2]))
	      | helper (T.GE, exp1, exp2, Ty.STRING) = Ex(F.externalCall("stringGE", [exp1, exp2]))
	      | helper (T.LT, exp1, exp2, Ty.STRING) = Ex(F.externalCall("stringLT", [exp1, exp2]))
	      | helper (T.GT, exp1, exp2, Ty.STRING) = Ex(F.externalCall("stringGT", [exp1, exp2]))
	      | helper (oper, exp1, exp2, _) = Cx(fn (t, f) => T.CJUMP(oper, exp1, exp2, t, f))
	in
	    helper(oper', left', right', ty)
	end
    end

fun transASSIGN (lhs, rhs) =
    let val lhs' = unEx lhs
	val rhs' = unEx rhs
    in Nx(T.MOVE(lhs', rhs'))
    end

(********** FUNCTIONS: nested->non-nested: explict frame management  **********)
fun procEntryExit (Level({rame, ...}, _), body) =
    let val body' = unEx body
	val body'' = F.procEntryExit1 (frame, T.MOVE(T.TEMP F.RV, body'))
    in fragments := F.PROC{frame=frame, body=body''}
    end;

fun getFrame (_, Top) = (ErrorMsg.error ~1 "can't find level through static links"; T.CONST 0)
  | getFrame (Top, _) = (ErrorMsg.error ~1 "functioned declared in outermost level"; T.CONST 0)
  | getFrame (decLevel as Level({parent = _, frame = _}, uniq1),
	      callLevel as Level({parent = par, frame = _}, uniq2)) =
    if uniq1 = uniq2
    then T.TEMP(F.FP)
    else T.MEM(getFrame(decLevel, par))
	      
fun transCALL (callLev, Level({parent, frame}, uniq), label, argList) =
    let val statLink = getFrame(parent, callLev)
    in
	Ex(T.CALL(T.NAME label, statLink::(map (fn e => unEx e) args)))
    end
  | transCALL (_, Top, label, argList) = Ex(T.CALL(T.NAME label, (map (fn e => unEx e) args)))

(********** LET, FUN: Nested Lexical Scoping -> Global Scope **********)
fun transLet (decs, body) = (* check if this works *)
    let val num_decs = List.length decs
	val body' = unEx body
	val decs' = map unNx decs
    in case num_decs of
	   0 => Ex(body')
	 | _ => Ex(T.ESEQ(seq decs', body'))
    end

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

fun fieldVar (recRef, index) = Ex(memInc(T.MEM(unEx recRef), T.CONST(index * F.wordSize)))

    
fun subscriptVar (arrRef, index) =
    let val indexTemp = Temp.newtemp()
	val arrTemp = Temp.newtemp()
	val errorLabel = Temp.newlabel()
	val successLabel = Temp.newlabel()
	val nextCheckLab = Temp.newlabel()
    in
	Ex(T.ESEQ(seq[
		       T.MOVE(T.TEMP indexTemp, unEx index),
		       T.MOVE(T.TEMP arrTemp, unEx arrRef),
		       T.CJUMP(T.GE, T.TEMP indexTemp, T.MEM(T.TEMP arrTemp), errorLabel, nextCheckLabel),
		       T.LABEL(nextCheckLabel),
		       T.CJUMP(T.LT, T.TEMP indexTemp, T.CONST 0, errorLabel, successLabel),
		       T.LABEL(errorLabel),
		       T.EXP(F.externalCall("exit", [T.CONST 1])),
		       T.LABEl(successLabel)
		   ],
		  memInc(T.MEM(T.TEMP arrTemp),
			 T.BINOP(T.MUL, T.BINOP(T.PLUS, T.TEMP indexTemp, T.CONST 1), T.CONST F.wordSize))))
    end
    
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
    end

fun transARRAY (size, init) =
    let val size' = unEx size
	val init' = unEx init
	val arrRef = Temp.newtemp()
    in
	Ex(T.ESEQ(seq[T.MOVE(T.TEMP arrRef, F.externalCall("initArray", [T.BINOP(T.PLUS, size', T.CONST 1), init'])),
		      T.MOVE(T.MEM(T.TEMP arrRef, size'))
		     ],
		  T.TEMP arrRef))
    end

(* pg 164, 288 *)
fun transRECORD (fieldList) =
    let val r = Temp.newtemp ()
	val alloc = T.MOVE(T.TEMP r,
			   F.externalCall("allocRecord",
					  [T.CONST(List.length fieldList)])
			  )
	fun iter (exp, (list, index)) = (list @ [T.MOVE(memInc(T.MEM(T.TEMP r), T.CONST (index * F.wordsize)), unEx exp)],
					 index + 1)
	val (moves, _) = foldl iter ([], 0) fieldList
    in
	Ex(T.ESEQ(seq(alloc::moves), T.TEMP r))
    end

fun transSEQ [] = Nx(T.EXP(T.CONST 0))
  | transSEQ [e] = e
  | transSEQ exps =
    let val first = List.take(exps, List.length(exps)-1)
	val first' = seq(map unNx first)
	val last = List.last exps
	val last' = unEx last
    in
	Ex(T.ESEQ(first', last'))
    end

end
(* Assume every variable escapes, keep in local frame and don't bother with findEscape *)

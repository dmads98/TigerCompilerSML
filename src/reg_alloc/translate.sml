structure Translate : TRANSLATE =
struct

structure Frame : FRAME = MipsFrame

structure T = Tree
structure F = Frame
structure A = Absyn
structure Ty = Types

datatype exp = Ex of T.exp
	     | Nx of T.stm
	     | Cx of Temp.label * Temp.label -> T.stm

(***** Proc and String Fragments *****)
type frag = F.frag
val fragments : frag list ref = ref [] (* frag list ref local *)
fun reset () = fragments := []
fun getResult (): (F.frag list) = !fragments

(***** Levels and Access *****)
datatype level = Top
	       | Level of {parent: level, frame: F.frame, uniq: unit ref}

type access = level * F.access

val outermost = Top

fun newLevel {parent, name, formals} =
    Level ({parent=parent, frame = F.newFrame({name=name, formals=true::formals}), uniq = ref()})

fun formals (Top : level) = []
  | formals (Level({parent, frame, uniq}) : level) =
    (map (fn acc => (Level({parent = parent, frame = frame, uniq = uniq}) : level, acc : F.access): access) (F.formals(frame)))

fun allocLocal (Top) (escape) = ((ErrorMsg.error ~1 "Cannot allocate variables in outermost level");
				 (Top, F.allocLocal(F.newFrame {formals = [],
								name = Temp.newlabel()}) escape))
  | allocLocal (Level({parent, frame, uniq})) (escape) =
    let val newAcc = F.allocLocal(frame)(escape)
	val retVal = (Level({parent = parent, frame = frame, uniq = uniq}), newAcc)
    in
	retVal
    end

(***** IR translation *****)
fun seq [s] = s
  | seq (a::l) = T.SEQ(a, seq l)
  | seq [] = (T.EXP(T.CONST(0)))
		 
(* exp -> T.exp *)
fun unEx (Ex e) = e
  | unEx (Cx genstm) =
    let val res = Temp.newtemp() (* register to store true/false result *)
	val t  = Temp.newlabel() and f = Temp.newlabel () (* Jump Labels: true and false *)
    in T.ESEQ(seq[T.MOVE(T.TEMPPOS res, T.CONST 1),
		  genstm(t, f),
		  T.LABEL f,
		  T.MOVE(T.TEMPPOS res, T.CONST 0),
		  T.LABEL t],
	      T.TEMP res)
    end
  | unEx (Nx (s as T.EXP(e))) = e
  | unEx (Nx (T.SEQ(s, T.SEQ ls))) = T.ESEQ(s, unEx (Nx (T.SEQ ls)))
  | unEx (Nx (T.SEQ(s, T.EXP ls))) = T.ESEQ(s, ls)
  | unEx (Nx x) = T.ESEQ(x, T.CONST 0) 

(* exp -> T.stm *)
fun unNx (Ex e) = T.EXP e (* Constructor T.EXP converts expression to a stm *)
  | unNx (Nx s) = s
  | unNx (c) = unNx(Ex(unEx(c)))
 

(* exp -> (Temp.label * Temp.label -> T.stm) pg 154 *)
fun unCx (Cx genstm) = genstm (* Return the genstm function *)
  | unCx (Nx _) = (ErrorMsg.error 0 "cannot unCx an Nx";
		   fn (x, y) => T.LABEL(Temp.newlabel()))
    | unCx (Ex e) =
      case e of
	  T.CONST 0 => (fn(t, f) => T.JUMP(T.NAME f, [f])) (* always false *)
	| T.CONST 1 => (fn(t, f) => T.JUMP(T.NAME t, [t])) (* always true *)
	| exp => (fn(t, f) => T.CJUMP(T.EQ, T.CONST 1, exp, t, f)) (* If exp=0, jump to false *)

fun convertToPos (T.TEMP t) = T.TEMPPOS t
  | convertToPos (T.MEM e) = T.MEMPOS e
  | convertToPos (T.ESEQ (s, e as T.MEM(_))) = T.ESEQPOS(s, convertToPos(e))
  | convertToPos (T.ESEQ (s, t as T.TEMP(_))) = T.ESEQPOS(s, convertToPos(t))
  | convertToPos _ = (ErrorMsg.error 0 "error in converting exp to pos";
		      T.TEMPPOS(Temp.newtemp()))

(********** IF WHILE FOR  **********)

(* Return an exp *)
fun transIFELSE (Ex(T.CONST 0), thenexp, elseexp) = Ex(unEx(elseexp))
  | transIFELSE (Ex(T.CONST 1), thenexp, elseexp) = Ex(unEx(thenexp))
  | transIFELSE (cond, thenexp, elseexp) = 
    let val cond' = unCx(cond)
	val then' = unEx(thenexp)
	val else' = unEx(elseexp)
	val res = Temp.newtemp ()
	val lt = Temp.newlabel ()
	and lf = Temp.newlabel ()
	and le = Temp.newlabel ()
    in Ex(T.ESEQ(seq[cond'(lt, lf),
		     T.LABEL lt,
		     T.MOVE(T.TEMPPOS res, then'),
		     T.JUMP(T.NAME le, [le]),
		     T.LABEL lf,
		     T.MOVE(T.TEMPPOS res, else'),
		     T.JUMP(T.NAME le, [le]),
		     T.LABEL le
		    ],
		 T.TEMP res))
    end

fun transIFTHEN (cond, thenexp) =
    let val cond' = unCx(cond)
	val then' = unEx(thenexp)
	val lt = Temp.newlabel ()
	and le = Temp.newlabel ()
    in Ex(T.ESEQ(seq[cond'(lt, le),
		     T.LABEL lt,
		     T.EXP(then'),
		     T.LABEL le
	     ], T.CONST 0))
    end


fun transBREAK label = Nx(T.JUMP(T.NAME label, [label]))

(* Return Nx (T.stm) *)
fun transWHILE (Ex(T.CONST 0), _, _) = Ex(T.CONST 0)
  | transWHILE (cond, body, lend) =
    let val cond' = unCx cond
	val body' = unNx body
	val ltest = Temp.newlabel ()
	and lstart = Temp.newlabel ()
    in Nx(seq[T.LABEL ltest,
	      cond' (lstart, lend),
	      T.LABEL lstart,
	      body',
	      T.JUMP(T.NAME ltest, [ltest]),
	      T.LABEL lend])
    end

fun transFORHelper (var, escape, lo, hi, body, lend) = 	
    let val lo' = unEx lo
	val hi' = unEx hi
	val body' = unNx body
	val lab1 = Temp.newlabel()
	val lab2 = Temp.newlabel()
	val loopVar = unEx var
    in
	Nx(seq[T.MOVE(convertToPos(loopVar), lo'),
	       T.CJUMP(T.LE, loopVar, hi', lab1, lend),
	       T.LABEL(lab1),
	       body',
	       T.CJUMP(T.LT, loopVar, hi', lab2, lend),
	       T.LABEL(lab2),
	       T.MOVE(convertToPos(loopVar), T.BINOP(T.PLUS, loopVar, T.CONST(1))),
	       T.JUMP(T.NAME(lab1), [lab1]),
	       T.LABEL(lend)
	  ])
    end

fun transFOR (var, escape, lo as Ex(T.CONST lVal), hi as Ex(T.CONST hVal), body, lend) =
    if lVal > hVal
    then Ex(T.CONST 0)
    else if lVal = hVal
    then Nx(seq[T.MOVE(convertToPos(unEx var), unEx lo), unNx body, T.LABEL(lend)])
    else transFORHelper(var, escape, lo, hi, body, lend)
  | transFOR (var, escape, lo, hi, body, lend) = transFORHelper(var, escape, lo, hi, body, lend)


fun getLogBase 0 = NONE
  | getLogBase 1 = SOME 0
  | getLogBase x = if x mod 2 = 0
		   then case getLogBase (x div 2) of SOME(a) => SOME(a + 1)
						  | NONE => NONE
		   else NONE

fun transBINOP (left, oper, right) =
    let val oper' =
	    case oper of
		A.PlusOp => T.PLUS
	      | A.MinusOp => T.MINUS
	      | A.TimesOp => T.MUL
	      | A.DivideOp => T.DIV
	fun binop (Ex(T.CONST x), T.PLUS, Ex(T.CONST y)) = Ex(T.CONST (x + y))
	  | binop (Ex(T.CONST 0), T.PLUS, exp) = exp
	  | binop (exp, T.PLUS, Ex(T.CONST 0)) = exp
	  | binop (Ex(T.CONST x), T.MINUS, Ex(T.CONST y)) = Ex(T.CONST (x - y))
	  | binop (exp, T.MINUS, Ex(T.CONST 0)) = exp
	  | binop (Ex(T.CONST 0), T.MUL, exp) = Ex(T.CONST 0)
	  | binop (exp, T.MUL, Ex(T.CONST 0)) = Ex(T.CONST 0)
	  | binop (Ex(T.CONST 1), T.MUL, exp) = exp
	  | binop (exp, T.MUL, Ex(T.CONST 1)) = exp
	  | binop (Ex(T.CONST x), T.MUL, Ex(T.CONST y)) = Ex(T.CONST (x * y))
	  | binop (Ex(T.CONST x), T.MUL, exp) =
	    (case getLogBase x of SOME(base) => Ex(T.BINOP(T.LSHIFT, unEx(exp), T.CONST(base)))
				| NONE => Ex(T.BINOP(T.MUL, unEx(Ex(T.CONST x)), unEx(exp)))) 
	  | binop (exp, T.MUL, Ex(T.CONST x)) =
	    (case getLogBase x of SOME(base) => Ex(T.BINOP(T.LSHIFT, unEx(exp), T.CONST(base)))
				| NONE => Ex(T.BINOP(T.MUL, unEx(exp), unEx(Ex(T.CONST x)))))
	  | binop (Ex(T.CONST x), T.DIV, Ex(T.CONST y)) = Ex(T.CONST (x div y))
	  | binop (exp1, op', exp2) = Ex(T.BINOP(op', unEx(exp1), unEx(exp2))) 
    in
	binop(left, oper', right)
    end

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
	let fun helper (T.EQ, exp1, exp2, Ty.STRING) = Ex(F.externalCall("tig_stringEqual", [exp1, exp2]))
	      | helper (T.NE, exp1, exp2, Ty.STRING) = Ex(T.BINOP(T.MINUS, T.CONST(1), F.externalCall("tig_stringEqual", [exp1, exp2])))
 	      | helper (T.LE, exp1, exp2, Ty.STRING) = Ex(F.externalCall("stringLE", [exp1, exp2]))
	      | helper (T.GE, exp1, exp2, Ty.STRING) = Ex(F.externalCall("stringGE", [exp1, exp2]))
	      | helper (T.LT, exp1, exp2, Ty.STRING) = Ex(F.externalCall("stringLT", [exp1, exp2]))
	      | helper (T.GT, exp1, exp2, Ty.STRING) = Ex(F.externalCall("stringGT", [exp1, exp2]))
	      | helper (T.LT, T.CONST(x), T.CONST(y), Ty.INT) = if x < y
							       then Ex(T.CONST 1)
							       else Ex(T.CONST 0)
	      | helper (T.GT, T.CONST(x), T.CONST(y), Ty.INT) = if x > y
							       then Ex(T.CONST 1)
							       else Ex(T.CONST 0)
	      | helper (T.GE, T.CONST(x), T.CONST(y), Ty.INT) = if x >= y
							       then Ex(T.CONST 1)
							       else Ex(T.CONST 0)
	      | helper (T.LE, T.CONST(x), T.CONST(y), Ty.INT) = if x <= y
							       then Ex(T.CONST 1)
							       else Ex(T.CONST 0)
	      | helper (T.EQ, T.CONST(x), T.CONST(y), Ty.INT) = if x = y
							       then Ex(T.CONST 1)
							       else Ex(T.CONST 0)
	      | helper (T.NE, T.CONST(x), T.CONST(y), Ty.INT) = if x <> y
							       then Ex(T.CONST 1)
							       else Ex(T.CONST 0)
	      | helper (oper, exp1, exp2, _) = Cx(fn (t, f) => T.CJUMP(oper, exp1, exp2, t, f))
	in
	    helper(oper', left', right', ty)
	end
    end

fun transASSIGN (lhs, rhs) =
    let val lhs' = unEx lhs
	val rhs' = unEx rhs
    in Nx(T.MOVE(convertToPos(lhs'), rhs'))
    end

(********** FUNCTIONS: nested->non-nested: explict frame management  **********)

fun procEntryExit ({level = lev, body = body}) =
    let val body' = unEx body
	val body'' = T.MOVE(T.TEMPPOS F.RV, body')
	val lFrame = case lev of Top => (ErrorMsg.error 0 "functioned is declared in outermost level";
					 F.newFrame({name = Temp.newlabel(), formals = []}))
				     | Level({frame = fr, parent = _, uniq = _}) => fr
	val frag' = F.PROC{body = F.procEntryExit1(lFrame, body''), frame = lFrame}
    in
	fragments := frag' :: (!fragments)
    end


fun follow  _ Top link = (ErrorMsg.error 0 "unable to follow static links";
			  link)
  | follow Top _ link = (ErrorMsg.error 0 "unable to follow static links";
			 link)
  | follow (dLevel as Level{parent = _, frame = _, uniq = uniqD})(useLevel as Level{parent = uParent, frame = _, uniq = uniqU}) link =
    if uniqD = uniqU
    then link
    else follow dLevel uParent (T.MEM link)
					      
fun transCALL (Level({parent, frame, uniq}), callLev, label, argList) =
    let val statLink = follow parent callLev (T.TEMP F.FP)
    in
	Ex(T.CALL(T.NAME label, statLink::(map unEx argList)))
    end
  | transCALL (Top, _, label, argList) = Ex(T.CALL(T.NAME label, (map unEx argList)))

(********** LET, FUN: Nested Lexical Scoping -> Global Scope **********)
fun transLET (decs, body) = (* check if this works *)
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
fun simpleVar ((lg, fAccess), lf) = Ex(F.exp (fAccess, follow lg lf (T.TEMP F.FP)))

fun fieldVar (recRef, index) = Ex(memInc(unEx recRef, T.CONST(index * F.wordSize)))

    
fun subscriptVar (arrRef, Ex(T.CONST 0)) = Ex(T.MEM(unEx arrRef))
  | subscriptVar (arrRef, index) =
    let val index' = unEx index
	val arrRef' = unEx arrRef
	val tmp = Temp.newtemp()
    in
	Ex(T.ESEQ(T.MOVE(T.TEMPPOS(tmp),
			 T.BINOP(T.PLUS, arrRef',

				 T.BINOP(T.MUL, T.BINOP(T.PLUS, index', T.CONST 1), T.CONST(F.wordSize)))),
				 T.MEM(T.TEMP tmp)))
    end
    
fun transINT (num : int) = Ex(T.CONST num);

(* T.STRING = (Temp.label * string) pg 163, 169 *)
fun transSTRING (str : string) =
    let fun isString (F.PROC _) = false
	  | isString (F.STRING (l, s)) = String.compare(s, str) = EQUAL
	val res = List.find(isString) (!fragments)
    in case res of
	   SOME (F.STRING(l, s)) => Ex(T.NAME l) (* return label if found *)
	 | _ => let val nl = Temp.newlabel () (* else, insert into frags as new string *)
		   in (fragments := F.STRING (nl, str) :: (!fragments);
		       Ex(T.NAME nl)) (* and return the new label *)
		   end
    end

fun transARRAY (size, init) =
    let val size' = unEx size
	val init' = unEx init
    in
	Ex(F.externalCall("tig_initArray", [size', init']))
    end

(* pg 164, 288 *)
fun transRECORD (fieldList) =
    let val r = Temp.newtemp ()
	val alloc = T.MOVE(T.TEMPPOS r,
			   F.externalCall("tig_allocRecord",
					  [T.CONST(List.length(fieldList) * F.wordSize)])
			  )
	fun iter (exp, el) = T.MOVE(T.MEMPOS(T.BINOP( T.PLUS, T.TEMP r, T.CONST(el * F.wordSize))),
				    unEx exp)
	fun initFields ([]) = [alloc]
	  | initFields (h :: l) = (iter(h, List.length l)):: initFields(l)
	fun help ([]) = T.EXP(T.CONST 0)
	  | help ([a]) = a
	  | help (el :: ls) = seq([el, help(ls)]) 
    in
	Ex(T.ESEQ(help(List.rev(initFields(fieldList))), T.TEMP r))
    end

fun transSEQ ls =
    let fun help [] = T.CONST 0
	  | help [e] = unEx e
	  | help (e :: list) = T.ESEQ(unNx e, help(list))
    in
	Ex(help(ls))
    end

fun transCONST x = Ex(T.CONST x)
	
end

structure Semant : SEMANT =
struct

type venv = Env.enventry Symbol.table
type tenv = Env.ty Symbol.table

structure A = Absyn
structure S = Symbol
structure E = Env
structure T = Types

(* Prog is an exp *)
fun transProg(exp: A.exp) : unit =
    (transExp(E.base_venv, E.base_tenv)(exp); ());

(* Expression base - only plus for now  pg 115, 121 *)
(* Tiger Expression List: Correlate with absyn.sml to implement
   l-value
   Valueless exp
   Nil					- Done
   Sequencing
   No Value
   Integer Literal			- Done
   String Literal 			- Done
   Negation 				-> Arithmetic 		- Done
   Function Call
   Arithmetic				- Done
   Comparison 				- Done
   String Comparison 			- Done
   Boolean Operators 			-> If-then-else
   Record Creation
   Array Creation
   Array and Record Assignment
   Extent
   Assignment
   If-then-else				- Done
   If-then
   While
   For
   Break
   Let
*)

(* Can use below for T.INT, T.STRING as is: Need to add more types *)
fun typeCheck (exp_ty, given_ty, pos) =
    if exp_ty <> given_ty then error pos "expected " ^ exp_ty ^ " saw " ^ given_ty else ();

(* An exp is {exp=, ty=} *)
fun checkArith (exp1, exp2, pos) =
    ((typeCheck (#ty exp1, T.INT, pos); typeCheck (#ty exp2, T.INT, pos)));

(* Both int or both string is okay *)
fun checkComp (exp1, exp2, pos) =
    case #ty exp1 of
	T.INT => ((); (typeCheck(#ty exp2, T.INT, pos)))
      | T.STRING => ((); (typeCheck(#ty exp2, T.STRING, pos)))
      | _ => error pos "comp: expected int or string for comparison";

(* Both int or string or array or record refs is okay *)
fun checkEq (exp1, exp2, pos) =
    case #ty exp1 of
	T.INT => ((); (typeCheck(#ty exp2, T.INT, pos)))
      | T.STRING => ((); (typeCheck(#ty exp2, T.STRING, pos)))
      | _ => error pos "eq: expected int, string, array or record for comparison";

fun transExp (venv, tenv) =
    let fun trexp A.NilExp = {exp=(), ty=T.NIL}
	  | trexp A.IntExp = {exp=(), ty=T.INT}
	  | trexp A.StringExp = {exp=(), ty=T.STRING}

	  | trexp A.VarExp(var) = trvar var (* Need to complete trvar function *)
	  | trexp A.AssignExp{var, exp, pos} =
	    let var' = trvar var;
		exp' = trexp exp;
	    in typeCheck(#ty var', #ty exp', pos); {exp=(), ty=T.UNIT}
	    end
	  | trexp A.IfExp{test, then', else', pos} =
	    (typeCheck(#ty test, T.INT, pos);
	     typeCheck(#ty then', #ty else', pos);
	     {exp=(), ty=(#ty then')})
	  
	  (* Arithmetic *)
	  | trexp (A.OpExp{left, oper = A.PlusOp, right, pos}) =
	    (checkArith(trexp left, trexp right, pos)
	     {exp=(), ty=Types.INT}
	    )
	  (* Uminus is just 0-num *)
	  | trexp (A.OpExp{left, oper=A.MinusOp, right, pos}) =
	    (checkArith(trexp left, trexp right, pos)
	     {exp=(), ty=Types.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.DivideOp, right, pos}) =
	    (checkArith(trexp left, trexp right, pos)
	     {exp=(), ty=Types.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.TimesOp, right, pos}) =
	    (checkArith(trexp left, trexp right, pos)
	     {exp=(), ty=Types.INT}
	    )
	  (*--Arithmetic--*)
	  (* Comparison -- int or string, to add string *)
	  | trexp (A.OpExp{left, oper=A.GtOp, right, pos}) =
	    (checkComp(trexp left, trexp right, pos)
	     {exp=(), ty=Types.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.GeOp, right, pos}) =
	    (checkComp(trexp left, trexp right, pos)
	     {exp=(), ty=Types.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.LtOp, right, pos}) =
	    (checkComp(trexp left, trexp right, pos)
	     {exp=(), ty=Types.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.LeOp, right, pos}) =
	    (checkComp(trexp left, trexp right, pos)
	     {exp=(), ty=Types.INT}
	    )
	  (* EqOp and NeqOp can take int, array, record on both sides *)
	  | trexp (A.OpExp{left, oper=A.EqOp, right, pos}) =
	    (checkEq(trexp left, trexp right, pos)
	     {exp=(), ty=Types.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.NeqOp, right, pos}) =
	    (checkEq(trexp left, trexp right, pos)
	     {exp=(), ty=Types.INT}
	    )
	  (*--Comparison--*)

	  (* transDec = Let expressions *)
	  | trexp (A.LetExp{decs, body, pos}) =
	    let val {venv = venv', tenv = tenv'} = transDecs (venv, tenv, decs)
						   (* Func yet to be defined *)
	    in transExp (venv', tenv') body
	    end
	  (* For expression needs to call transExp since it needs to modify the env *)

	  | trexp (A.RecordExp{left, oper = A.PlusOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )

	and trvar (A.SimpleVar(id, pos)) =
	    (case Symbol.look(venv, id)
	      of SOME (E.VarEntry{ty}) => {exp=(), ty = actual_ty ty}
	       | NONE => (error pos ("undefined variable " ^ S.name id);
			  exp=(), ty=Types.INT))
    in
	{exp=(), ty=Types.INT}
    end
end


fun transDecs (venv, tenv, decs) =
    let val venv' = venv
	val tenv' = tenv
    in
	{venv= venv', tenv= tenv'} (* Add update *)
    end

fun transDec (venv, tenv, A.VarDec{name, typ=NONE, init, ...}) =   (* var dec *)
    let val {exp, ty} = transExp(venv, tenv, init)
    in {tenv = tenv,
       venv = S.enter{venv, name, E.VarEntry}}
    end
  (* type dec *)
  | transDec (venv, tenv, A.TypeDec[{name, ty}]) =
    {venv = venv,
    tenv = S.enter(tenv, name, transTy(tenv, ty))}

  (* function dec -- needs to be improved *)
  | transDec (venv, tenv, A.FunctionDec[{name, params, body, pos,
					 result = SOME(rt, pos)}]) =
    let val SOME(result_ty) = S.look(tenv, rt)
	fun transparam{name, typ, pos} =
	    case S.look(tenv, typ)
	     of SOME t => {name=name, ty=t}
	val params' = map transparam params
	val venv' = S.enter(venv, name,
			    E.FunEntry{formals= map #ty params',
				       result=result_ty})
	fun enterparam ({name, ty}, venv) =
	    S.enter(venv, name, S.VarEntry{access=(), ty=ty})
	val venv'' = fold enterparam params' venv'
    in transExp (venv'', tenv) body;
       {venv=venv', tenv=tenv'}
    end

structure Semant : SEMANT =
struct

type venv = Env.enventry Symbol.table
type tenv = Env.ty Symbol.table

structure A = Absyn
structure S = Symbol
structure E = Env

(* Prog is an exp *)
fun transProg(exp: A.exp) : unit =
    (transExp(E.base_venv, E.base_tenv)(exp); ());

(* Type check an int *)
fun checkInt({exp, ty}, pos) =
    case ty of Types.INT => ()
	     | _ => error pos "integer required";

(* Expression base - only plus for now  pg 115, 121 *)
(* Tiger Expression List: Correlate with absyn.sml to implement
   l-value
   Valueless exp
   Nil
   Sequencing
   No Value
   Integer Literal
   String Literal
   Negation
   Function Call
   Arithmetic
   Comparison
   String Comparison
   Boolean Operators
   Precedence of Operators
   Associativity of Operators
   Record Creation
   Array Creation
   Array and Record Assignment
   Extent
   Assignment
   If-then-else
   If-then
   While
   For
   Break   
   Let
   Parentheses
*)
fun transExp(venv, tenv) =
    (* Arithmetic *)
    let fun trexp (A.OpExp{left, oper = A.PlusOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )
	  (* Uminus is just 0-num *)
	  | trexp (A.OpExp{left, oper=A.MinusOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}}
	  | trexp (A.OpExp{left, oper=A.DivideOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}}
	  | trexp (A.OpExp{left, oper=A.TimesOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}}
	  (*--Arithmetic--*)
	  (* Comparison *)
	  | trexp (A.OpExp{left, oper=A.GtOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}}
	  | trexp (A.OpExp{left, oper=A.GeOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}}
	  | trexp (A.OpExp{left, oper=A.LtOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}}
	  | trexp (A.OpExp{left, oper=A.LeOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}}
	  (* EqOp and NeqOp can take int, array, record on both sides *)
	  | trexp (A.OpExp{left, oper=A.EqOp, right, pos}) =
	    (checkIAR(trexp left, pos);
	     checkIAR(trexp right, pos);
	     {exp=(), ty=Types.INT}}
	  | trexp (A.OpExp{left, oper=A.NeqOp, right, pos}) =
	    (checkIAR(trexp left, pos);
	     checkIAR(trexp right, pos);
	     {exp=(), ty=Types.INT}}
	  (*--Comparison--*)

	  (* transDec = Let expressions *)
	  | trexp (A.LetExp{decs, body, pos}) =
	    let val {venv = venv', tenv = tenv'} = transDecs(venv, tenv, decs)
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

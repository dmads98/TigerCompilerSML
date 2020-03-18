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
fun transExp(venv, tenv) =
    let fun trexp (A.OpExp{left, oper = A.PlusOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )

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

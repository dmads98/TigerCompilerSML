structure A = Absyn
structure S = Symbol

structure FindEscape : sig val findEscape: A.exp -> unit
			   val reset : unit -> unit
			   val getRefs : unit -> (bool ref) list ref
		       end =

struct

type depth = int
type escEnv = (depth * bool ref) S.table
val escRefs = ref [] : (bool ref) list ref

fun traverseVar (env:escEnv, d:depth, s:A.var) : unit =
    let
	fun trVar (A.FieldVar(var, symb, pos)) =
	    let
	    in
		(
		  
		  (* case var of *)
		  (*     SimpleVar (sym, pos) =>  *)
		  (*     print ("var sym: " ^ S.name symb ^ " in trvar\n") *)
		  (*   | FieldVar () =>  *)
	      trVar(var)
	    )
	    end
		   
	  | trVar (A.SubscriptVar(var, exp, pos)) = (trVar(var);
						     traverseExp(env, d, exp))
	  | trVar (A.SimpleVar(id, pos)) =
	    (case S.look(env, id) of
		 SOME(actualDepth, escape) =>
		 (if actualDepth < d
		  then escape := true
		  else ()
		 )
	       | NONE => ())
    in
	trVar(s)
    end
and traverseExp (env:escEnv, d:depth, s:A.exp) :unit =
    let
	fun trExp (A.NilExp) = ()
	  | trExp (A.IntExp(num)) = ()
	  | trExp (A.StringExp(str)) = ()
	  | trExp (A.OpExp{left, oper, right, pos}) = (trExp(left);
						       trExp(right))
	  | trExp (A.BreakExp(pos)) = ()
	  | trExp (A.CallExp{func, args, pos}) = List.app trExp args
	  | trExp (A.LetExp{decs, body, pos}) = traverseExp(traverseDecs(env, d, decs), d, body)
	  | trExp (A.ForExp{var, escape, lo, hi, body, pos}) =
	    let val env' = S.enter(env, var, (d, escape))
	    in
		(traverseExp (env', d, lo);
		 traverseExp(env', d, hi);
		 traverseExp(env', d, body))
	    end
	  | trExp (A.WhileExp{test, body, pos}) = (trExp(test);
						   trExp(body))
	  | trExp (A.SeqExp(expList)) = List.app (fn (exp, pos) => trExp(exp)) expList
	  | trExp (A.AssignExp{var, exp, pos}) = (traverseVar(env, d, var);
						  trExp(exp))
	  | trExp (A.IfExp{test, then', else' = SOME(expr), pos}) = (trExp(test);
								     trExp(then');
								     trExp(expr))
	  | trExp (A.IfExp{test, then', else' = NONE, pos}) = (trExp(test);
							       trExp(then'))
	  | trExp (A.ArrayExp{typ = t, size = sz, init = init, pos = pos}) = (trExp(sz);
									      trExp(init))
	  | trExp (A.RecordExp{fields = fs, typ = typ, pos = pos}) = List.app (fn (symb, exp, pos) => trExp(exp)) fs
	  | trExp (A.VarExp(var)) = traverseVar(env, d, var) 
    in
	trExp(s)
    end
and traverseDecs (env, d, s: A.dec list) : escEnv =
    let fun checkDecs (A.VarDec{name, escape, typ, init, pos}, env) =
	    let val env' = S.enter(env, name, (d, escape))
	    in
		(escRefs := (escape) :: (!escRefs);
		 traverseExp(env', d, init);
		 env')	    
	    end

	  | checkDecs (A.TypeDec(decList), env) = env
	  | checkDecs (A.FunctionDec(decList), env) =
	    let fun updateEnv ({name, escape, typ, pos}, env) =
		    (escRefs := (escape) :: !escRefs;
		     S.enter(env, name, (d + 1, escape)))
		fun checkDecs {name, params, result, body, pos} =
		    let val updatedEnv = foldl updateEnv env params
		    in
			traverseExp(updatedEnv, d + 1, body)
		    end
	    in
		List.app checkDecs decList;
		env
	    end
    in
	foldl checkDecs env s
    end

fun reset () = escRefs := []
fun getRefs () = escRefs
fun findEscape (prog: A.exp) : unit = traverseExp (S.empty, 0, prog)
end


structure A = Absyn
structure S = Symbol

structure FindEscape : sig val findEscape: A.exp -> unit
		       end =

struct type depth = int
       type escEnv = (depth * bool ref) S.table

       fun traverseVar (env:escEnv, d:depth, s:A.var) : unit =
	   let
	       fun trVar (A.FieldVar(var, symb, pos)) = trVar(var)
		 | trVar (A.SubscriptVar(var, exp, pos)) = (trVar(var);
							    traverseExp(env, d, exp))
		 | trVar (A.SimpleVar(id, pos)) = (case S.look(env, id) of SOME((actualDepth, escape)) => if d > actualDepth
													  then escape := true
													  else ()
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
		 | trExp (A.ForExp{var, escape, lo, hi, body, pos}) = (traverseExp(S.enter(env, var, (d, escape)), d, body);
								       trExp(lo);
								       trExp(hi))
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
	   let fun checkDecs (A.VarDec{name, typ, init, escape, pos}, env) = S.enter(env, name, (d, escape))
		 | checkDecs (A.TypeDec(decList), env) = env
		 | checkDecs (A.FunctionDec(decList), env) =
		   let fun updateEnv ({name, params, result, body, pos}) =
			   traverseExp(foldl (fn ({name, escape, typ, pos}, env) => S.enter(env, name, (d + 1, escape))) env params, d + 1, body)
		   in
		       (List.app updateEnv decList;
			env)
		   end
	   in
	       foldl checkDecs env s
	   end

       fun findEscape (prog: A.exp) : unit = traverseExp (S.empty, 0, prog)
end


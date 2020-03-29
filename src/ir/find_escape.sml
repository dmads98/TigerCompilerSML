structure A = Absyn
structure S = Symbol
structure E = Env
structure T = Types

structure FindEscape : sig val findEscape: A.exp -> unit
		       end =
struct type depth = int
       type escEnv = (depth * bool ref) S.table

       fun traverseVar (env:escEnv, d:depth, s:A.var) = ;
       and traverseExp (env:escEnv, d:depth, s:A.exp) = ;
       and traverseDecs (env, d, s: A.dec list) : escEnv = ;

       fun findEscape (prog: A.exp) : unit = ;
end
(* Complete: Needs to happen before the semant phase? *)

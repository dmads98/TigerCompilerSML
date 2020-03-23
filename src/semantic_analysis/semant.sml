structure A = Absyn
structure S = Symbol
structure E = Env
structure T = Types

structure Semant : SEMANT =
struct

type venv = E.enventry S.table
type tenv = E.ty S.table
type ty = T.ty
type expty = {exp: Translate.exp, ty: ty}	 

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

fun typeHelper (tenv, typ: ty, pos) = typ
  | typeHelper (tenv, T.NAME(symb, ref), pos) =
    let fun checkHelper (SOME typ) = typeHelper(tenv, typ, pos)
	  | checkHelper (NONE) = (ErrorMsg.error pos ("type " ^ (S.name symb) ^ " not yet defined"); T.INT)
    in
	checkHelper(!ref)
    end

fun getActualType (tenv, symb : S.symbol, pos) =
    let val value = S.look(tenv, symb)
    in
	case value of NONE => (ErrorMsg.error pos ("type " ^ (S.name symb) ^ " not yet defined"); T.INT)
		    | SOME(typ) => typeHelper(tenv, typ, pos)
    end

fun isSameType (tenv, pos, typ1:ty, typ2:ty) = (typeHelper(tenv, typ1, pos) = typeHelper(tenv, typ2, pos))
  | isSameType (tenv, pos, T.NIL, T.RECORD (typ, ref)) = true
  | isSameType (tenv, pos, T.RECORD (typ, ref), T.NIL) = true
  | isSameType (tenv, pos, T.RECORD (typ1, ref1), T.RECORD (typ2, ref2)) = ref1 = ref2
  | isSameType (tenv, pos, T.ARRAY(typ1, ref1), T.ARRAY(typ2, ref2)) = (ref1 = ref2) andalso
								       (isSameType(tenv, pos, typeHelper(tenv, typ1,pos), typeHelper(tenv, typ2, pos)))

(* An exp is {exp=, ty=} *)
fun checkArith (exp1, exp2, pos) =
    ((typeCheck (#ty exp1, T.INT, pos); typeCheck (#ty exp2, T.INT, pos)));

fun checkInt ({exp, ty}, pos) = case ty of T.INT => ()
					 | ty => ErrorMsg.error pos ("integer expected, found " ^ getType(ty)); 

(* Both int or both string is okay *)
fun checkComp ({exp = exp1, ty = T.INT}, {exp = exp2, ty = T.INT}, pos) = ()
  | checkComp ({exp = exp1, ty = T.STRING}, {exp = exp2, ty = T.STRING}, pos) = ()
  | checkComp ({exp = exp1, ty = _}, {exp = exp2, ty = _}, pos) = ErrorMsg.error pos "expected a matching int/str for comparison";

(* Both int or string or array or record refs is okay *)
fun checkEq ({exp = exp1, ty = T.INT}, {exp = exp2, ty = T.INT}, pos) = ()
  | checkEq ({exp = exp1, ty = T.STRING}, {exp = exp2, ty = T.STRING}, pos) = ()
  | checkEq ({exp = exp1, ty = T.NIL}, {exp = exp2, ty = T.NIL}, pos) = ()
  | checkEq ({exp = exp1, ty = T.NIL}, {exp = exp2, ty = T.RECORD _}, pos) = ()
  | checkEq ({exp = exp1, ty = T.RECORD _}, {exp = exp2, ty = T.NIL}, pos) = ()
  | checkEq ({exp = exp1, ty = T.RECORD(_, ref1)}, {exp = exp2, ty = T.RECORD(_, ref2)}, pos) = if ref1 <> ref2 then ErrorMsg.error pos ("expected matching record types to check for equality"; ()) else ()
  | checkEq ({exp = exp1, ty = T.ARRAY(_, ref1)}, {exp = exp2, ty = T.ARRAY(_, ref2)}, pos) = if ref1 <> ref2 then ErrorMsg.error pos ("expected matching array types to check for equality"; ()) else ()   
  | checkEq ({exp = exp1, ty = _}, {exp = exp2, ty = _}, pos) = ErrorMsg.error pos "expected a matching int/str for comparison";

fun cycleExists (tenv, symb, list) =
    case S.look(tenv, symb) of SOME(T.NAME(_, ref)) =>
			       (case !ref of SOME(T.NAME(sym, _)) => if member(sym, list)
								     then true
								     else cycleExists(tenv, symb, list @ [symb])
					   | _ => false)
			     | SOME(_) => false
			     | NONE => false

fun getType (T.NIL) = "NIL"
  | getType (T.STRING) = "STRING"
  | getType (T.INT) = "INT"
  | getType (T.UNIT) = "UNIT"
  | getType (T.RECORD(ls, un)) = "RECORD"
  | getType (T.NAME(sym, ty)) = "NAME"
  | getType (T.ARRAY(ty, un)) = "ARRAY OF " ^ getType(ty)

fun member (x : S.symbol, nil) = false
  | member (x, y::ys) = x=y orelse member (x,ys); 

fun transExp (venv, tenv) =
    let fun trexp (A.NilExp) = {exp=(), ty=T.NIL}
	  | trexp (A.IntExp(int)) = {exp=(), ty=T.INT}
	  | trexp (A.StringExp(str)) = {exp=(), ty=T.STRING}
					   
	  | trexp A.VarExp(var) = trvar var (* Need to complete trvar function *)
	  | trexp (A.AssignExp{var, exp, pos}) =
	    let val var' = trvar var;
		val exp' = trexp exp;
	    in (typeCheck(#ty var', #ty exp', pos); {exp=(), ty=T.UNIT})
	    end
	  | trexp A.IfExp{test, then', else', pos} =
	    (typeCheck(#ty test, T.INT, pos);
	     typeCheck(#ty then', #ty else', pos);
	     {exp=(), ty=(#ty then')})
		
	  (* Arithmetic *)
	  | trexp (A.OpExp{left, oper = A.PlusOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )
	  (* Uminus is just 0-num *)
	  | trexp (A.OpExp{left, oper=A.MinusOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.DivideOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.TimesOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )
	  (*--Arithmetic--*)
	  (* Comparison -- int or string *)
	  | trexp (A.OpExp{left, oper=A.GtOp, right, pos}) =
	    (checkComp(trexp left, trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.GeOp, right, pos}) =
	    (checkComp(trexp left, trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.LtOp, right, pos}) =
	    (checkComp(trexp left, trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.LeOp, right, pos}) =
	    (checkComp(trexp left, trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )
	  (* EqOp and NeqOp can take int, array, record on both sides *)
	  | trexp (A.OpExp{left, oper=A.EqOp, right, pos}) =
	    (checkEq(trexp left, trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.NeqOp, right, pos}) =
	    (checkEq(trexp left, trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )
	  (*--Comparison--*)
		
	  (* transDec = Let expressions *)
	  | trexp (A.LetExp{decs, body, pos}) =
	    let val {venv = venv', tenv = tenv'} = transDecs (venv, tenv, decs)
							     (* Func yet to be defined *)
	    in
		transExp (venv', tenv') body
	    end
		
	  | trexp (A.SeqExp (exprList)) =
	    let fun listCheckHelp [] = {exp = (), ty = T.UNIT}
		  | listCheckHelp [(exp, pos)] = trexp(exp)
		  | listCheckHelp ((exp, pos) :: list) = (trexp(exp); listCheckHelp(list))
	    in
		listCheckHelp(exprList)
	    end
		
	  (* For expression needs to call transExp since it needs to modify the env *)
		
	  | trexp (A.RecordExp{left, oper = A.PlusOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=Types.INT}
	    )
	(*| trexp (A.ArrayExp{}) = *)
		
	and trvar (A.SimpleVar(id, pos)) =
	    (case Symbol.look(venv, id)
	      of SOME (E.VarEntry{ty}) => {exp=(), ty = actual_ty ty}
	       | NONE => (error pos ("undefined variable " ^ S.name id);
			  exp=(), ty=Types.INT))
    in
	{exp=(), ty=Types.INT}
    end
	
and transDecs (venv, tenv, []) = {venv = venv, tenv = tenv}
  | transDecs (venv, tenv, decs)  = 
    let fun trdec (A.VarDec{name, escape, typ = SOME((assignType, typPos)), init, pos}, {venv, tenv}) =
	    let val {exp, ty = typeFound} = transExp(venv, tenv, init)
	    in
		if isSameType(tenv, pos, getActualType(tenv, assignType, typPos), typeHelper(tenv, typeFound, pos))
		then {venv = S.enter(venv, name, E.VarEntry {ty = typeFound}), tenv = tenv}
		else (ErrorMsg.error pos ("body of var declaration has incorrect type");
		      {venv = venv, tenv = tenv})
	    end
	  | trdec (A.VarDec{name, escape, typ = NONE, init, pos}, {venv, tenv}) =
	    let val {exp, ty = typeFound} = transExp(venv, tenv, init)
	    in
		if typeFound <> T.NIL
		then {venv = S.enter(venv, name, E.VarEntry {ty = typeFound}), tenv = tenv}
		else (ErrorMsg.error pos ("cannot assign var to nil for a non-record type");
		      {venv = venv, tenv = tenv})
	    end


		
	  | trdec (A.FunctionDec(decList), {venv, tenv}) =
	    let
		fun createTempVenv (params, venv) =
		    let fun insertIntoVenv({name, escape, typ, pos}, venv) = S.enter(venv, name, E.VarEntry{ty = getActualType(tenv, typ, pos)})
		    in
			foldl insertIntoVenv venv params
		    end
			
		fun checkFuncDec (venv, tenv, {name, params, result = NONE, body, pos}) =
		    let val tempVenv = createTempVenv(params, venv)
		    in
			if isSameType(tenv, pos, #ty(transExp(tempVenv, tenv, body)), T.UNIT)
			then {venv = venv, tenv = tenv}
			else (ErrorMsg.error pos ("function body and return type do not match"); {venv = venv, tenv = tenv})
		    end
		  | checkFuncDec (venv, tenv, {name, params, result = SOME(retType, pos2), body, pos}) =
		    let val tempVenv = createTempVenv(params, venv)
		    in
			if isSameType(tenv, pos, #ty(transExp(tempVenv, tenv, body)), getActualType(tenv, retType, pos))
			then {venv = venv, tenv = tenv}
			else (ErrorMsg.error pos ("function body and return type do not match"); {venv = venv, tenv = tenv})
		    end

		fun updateVenv ({name, params, result = NONE, body, pos}, venv, tenv) =
		    let val paramTypes = map (fn {name, escape, typ = symb, pos} => getActualType(tenv, symb, pos)) params
		    in
			{venv = S.enter(venv, name, E.FunEntry{formals = paramTypes, result = T.UNIT}), tenv = tenv}
		    end
		  | updateVenv({name, params, result = SOME(sym, pos2), body, pos}, venv, tenv) =
		    let val paramTypes = map (fn {name, escape, typ = symb, pos} => getActualType(tenv, symb, pos))params
		    in
			{venv = S.enter(venv, name, E.FunEntry{formals = paramTypes, result = getActualType(tenv, sym, pos)}), tenv = tenv}
		    end 

		fun handleFuncNames (venv, tenv, ls, []) = {venv = venv, tenv = tenv}
		  | handleFuncNames (venv, tenv, ls, [fDec]) =
		    if member (#name(fDec), ls)
		    then (ErrorMsg.error (#pos(fDec)) ("function " ^ S.name(#name(fDec)) ^ " already declared in block"); {tenv = tenv, venv = venv})
		    else updateVenv(fDec, venv, tenv)
		  | handleFuncNames (venv, tenv, ls, fDec::decs) =
		    let val {venv = venv', tenv = tenv'} =
			    if member(#name(fDec), ls)
			    then (ErrorMsg.error (#pos(fDec)) ("function " ^ S.name(#name(fDec)) ^ " already declared in block"); {tenv = tenv, venv = venv})
			    else updateVenv(fDec, venv, tenv)
		    in
			handleFuncNames(venv', tenv', ls @ [#name(fDec)], decs)
		    end

		fun handleFuncDecs (venv, tenv, []) = {venv = venv, tenv = tenv}
		  | handleFuncDecs (venv, tenv, [fDec]) = checkFuncDec(venv, tenv, fDec)
		  | handleFuncDecs (venv, tenv, fDec::list) =
		    let val {venv = venv', tenv = tenv'} = checkFuncDec(venv, tenv, fDec)
		    in
			handleFuncDecs(venv', tenv', list)
		    end
			
	    in
		let val {venv = venv', tenv = tenv'} = handleFuncNames(venv, tenv, [], decList)
		in
		    handleFuncDecs(venv', tenv', decList)
		end
	    end


		
	  | trdec (A.TypeDec(decList), {venv, tenv}) =
	    let fun checkTypeDec (venv, tenv, {name, ty = A.ArrayTy(symb, tyPos), pos}) =
		    let val symbVal = S.look(tenv, symb)
			val nameVal = S.look(tenv, name)
			val actualType = if isSome(symbVal)
					 then valOf(symbVal)
					 else (ErrorMsg.error pos ("type " ^ S.name symb ^ " not yet declared"); T.INT)
			val typeOption = SOME(T.ARRAY(actualType, ref ()))
			fun setRef(newRef, T.NAME(name, oldRef)) = ((oldRef := newRef); ())
			  | setRef (_) = () 
		    in
			setRef(valOf(nameVal), typeOption);
			{venv = venv, tenv = tenv}
		    end
		  | checkTypeDec (venv, tenv, {name, ty = A.NameTy(symb, tyPos), pos})  = 
		    let val symbVal = S.look(tenv, symb)
			val nameVal = S.look(tenv, name)
			val actualType = if isSome(symbVal)
					 then valOf(symbVal)
					 else (ErrorMsg.error pos ("type " ^ S.name symb ^ " not yet declared"); NONE)
			fun setRef(newRef, T.NAME(name, oldRef)) = ((oldRef := newRef); ())
			  | setRef (_) = () 
		    in
			setRef(valOf(nameVal), actualType);
			if cycleExists (tenv, name, [])
			then (ErrorMsg.error pos "declared type causes a cycle"; setRef(valOf(nameVal), NONE))
			else ();
			{venv = venv, tenv = tenv}
		    end
		  | checkTypeDec (venv, tenv, {name, ty = A.RecordTy(fields), pos}) =
		    let val nameVal = S.look(tenv, name)
			fun checkFields ({name, escape, typ, pos}, (nameList, fieldList)) =
			    let val curType = S.look(tenv, typ)
				val actualType = if isSome(curType)
						 then valOf(curType)
						 else (ErrorMsg.error pos ("type " ^ S.name typ ^ " not yet declared"); T.INT)
			    in
				if member(name, nameList)
				then (ErrorMsg.error pos ("field " ^ S.name(name) ^ " in record has already been declared"); (nameList, fieldList))
				else (nameList @ [name], fieldList @ [(name, actualType)])
			    end
			val (fields, _) = foldl checkFields ([],[]) fields
			val typeOption = SOME(T.RECORD(fields, ref ()))
			fun setRef(newRef, T.NAME(name, oldRef)) = ((oldRef := newRef); ())
			  | setRef (_) = () 
		    in
			setRef(valOf(nameVal), typeOption);
			{venv = venv, tenv = tenv}
		    end

			
		fun handleTypeNames (venv, tenv, ls, []) = {venv = venv, tenv = tenv}
		  | handleTypeNames (venv, tenv, ls, [tDec]) =
		    if member (#name(tDec), ls)
		    then (ErrorMsg.error (#pos(tDec)) ("type " ^ S.name(#name(tDec)) ^ " already declared in block"); {tenv = tenv, venv = venv})
		    else {venv = venv, tenv = S.enter(tenv, #name(tDec), T.NAME(#name(tDec), (ref NONE)))}
		  | handleTypeNames (venv, tenv, ls, tDec::decs) =
		    let val {venv = venv', tenv = tenv'} =
			    if member (#name(tDec), ls)
			    then (ErrorMsg.error (#pos(tDec)) ("type " ^ S.name(#name(tDec)) ^ " already declared in block"); {tenv = tenv, venv = venv})
			    else {venv = venv, tenv = S.enter(tenv, #name(tDec), T.NAME(#name(tDec), (ref NONE)))}
		    in
			handleTypeNames(venv', tenv', ls @ [#name(tDec)], decs)
		    end

		fun handleTypeDecs (venv, tenv, []) = {venv = venv, tenv = tenv}
		  | handleTypeDecs (venv, tenv, [tDec]) = checkTypeDec(venv, tenv, tDec)
		  | handleTypeDecs (venv, tenv, tDec::list) =
		    let val {venv = venv', tenv = tenv'} = checkTypeDec(venv, tenv, tDec)
		    in
			handleTypeDecs(venv', tenv', list)
		    end
	    in
		let val {venv = venv', tenv = tenv'} = handleTypeNames(venv, tenv, [], decList)
		in
		    handleFuncDecs(venv', tenv', decList)
		end
	    end

		
		let val {exp, ty} = transExp(venv, tenv, init)
		in {tenv = tenv,
		    venv = S.enter{venv, name, E.VarEntry}}
		end

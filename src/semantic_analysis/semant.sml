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
val loopLevel = ref 0

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
(*fun typeCheck (exp_ty, given_ty, pos) =
    if exp_ty <> given_ty then error pos "expected " ^ exp_ty ^ " saw " ^ given_ty else ();*)

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

fun arrayType (T.Array(typ, ref), pos) = typ
  | arrayType (_, pos) = (ErrorMsg.error pos "expression does not have type of array";
			  T.INT)

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
  | checkEq ({exp = exp1, ty = T.RECORD(_, ref1)}, {exp = exp2, ty = T.RECORD(_, ref2)}, pos) =
    if ref1 <> ref2
    then ErrorMsg.error pos ("expected matching record types to check for equality"; ())
    else ()
  | checkEq ({exp = exp1, ty = T.ARRAY(_, ref1)}, {exp = exp2, ty = T.ARRAY(_, ref2)}, pos) =
    if ref1 <> ref2
    then ErrorMsg.error pos ("expected matching array types to check for equality"; ())
    else ()   
  | checkEq ({exp = exp1, ty = _}, {exp = exp2, ty = _}, pos) = ErrorMsg.error pos "expected matching types to check for equality";

fun cycleExists (tenv, symb, list) =
    case S.look(tenv, symb)
     of SOME(T.NAME(_, ref)) =>
	(case !ref of
	     SOME(T.NAME(sym, _)) => if member(sym, list)
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
    let fun trexp (A.NilExp) = {exp = (), ty = T.NIL}
	  | trexp (A.IntExp(int)) = {exp = (), ty = T.INT}
	  | trexp (A.StringExp(str)) = {exp = (), ty = T.STRING}
	  | trexp A.VarExp(var) = trvar(var)
	  | trexp (A.AssignExp{var, exp, pos}) =
	    (if isSameType(tenv, pos, typeHelper(tenv, #ty(trvar(var)), pos),
			   typeHelper(tenv, #ty(trexp exp), pos))
	     then ()
	     else ErrorMsg.error pos "variable and expression types do not match";
	     {exp = (), ty = T.UNIT})
		
	  | trexp (A.IfExp{test, then', else', pos}) =
	    (checkInt(trexp test, pos);
	     if isSome(else')
	     then
		 (if isSameType(tenv, pos, typeHelper(tenv, #ty(trexp then'), pos), typeHelper(tenv, #ty(trexp (valOf(else'))), pos))
		  then ()
		  else ErrorMsg.error pos "then and else statements return types do not match";
		  {exp = (), ty = typeHelper(tenv, #ty(trexp then'), pos)})
	     else
		 (if isSameType(tenv, pos, typeHelper(tenv, #ty(trexp then'), pos), T.UNIT)
		  then ()
		  else ErrorMsg.error pos "then statement does not have type UNIT";
		  {exp = (), ty = T.UNIT})
	    )

	  | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
	    (checkInt(trexp lo, pos);
	     checkInt(trexp hi, pos);
	     loopLevel := !loopLevel + 1;
	     let val tempVenv = S.enter(venv, var, E.VarEntry{ty = T.INT})
	     in
		 if isSameType(tenv, pos, #ty(transExp(tempVenv, tenv) body), T.UNIT)
		 then (loopLevel := !loopLevel - 1;
		       {exp = (), ty = T.UNIT})
		 else (loopLevel := !loopLevel - 1;
		       ErrorMsg.error pos "body of for loop should have type UNIT";
		       {exp = (), ty = T.INT})
	     end
	    )

	  | trexp (A.WhileExp{test, body, pos}) =
	    (checkInt(trexp test, pos);
	     loopLevel := !loopLevel + 1;
	     if isSameType(tenv, pos, #ty(transExp(venv, tenv) body), T.UNIT)
	     then (loopLevel := !loopLevel - 1;
		   {exp = (), ty = T.UNIT})
	     else (loopLevel := !loopLevel - 1;
		   ErrorMsg.error pos "body of while loop should have type UNIT";
		   {exp = (), ty = T.INT})
	    )

	  | trexp (A.BreakExp(pos)) =
	    (if (!loopLevel > 0)
	     then {exp = (), ty = T.UNIT}
	     else ();
	     if (!loopLevel = 0)
	     then (ErrorMsg.error pos "break expression exists outside of a loop";
		   {exp = (), ty = T.UNIT})
	     else (ErrorMsg.error pos "loop depth negative ?")
	    )

	  | trexp (A.CallExp{func, args, pos}) = 
	    let fun checkParamTypes (index, arg) =
		    (if isSameType(tenv, pos, typeHelper(tenv, arg, pos), typeHelper(tenv, (#ty(trexp(List.nth(args, index)))) , pos))
		     then ()
		     else ErrorMsg.error pos "parameter types do not match";
		     index + 1)
		val entry = case S.look(venv, func) of SOME(E.VarEntry{ty}) => ((ErrorMsg.error pos "identifier is a variable not a function"); E.FunEntry{formals = [], result = T.UNIT})
						     | SOME(E.FunEntry{formals, result}) => valOf(S.look(venv, func))
						     | _ => ((ErrorMsg.error pos "function is undefined"); E.FunEntry{formals = [], result = T.UNIT})
	    in
		if List.length(#formals(entry)) <> List.length(args)
		then (ErrorMsg.error pos "number of arguments do not match"; ())
		else (foldl checkParamTypes 0 #formals(entry); ());
		{exp = (), ty = typeHelper(tenv, #result(entry), pos)}
	    end
		
	  (* Arithmetic *)
	  | trexp (A.OpExp{left, oper = A.PlusOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=T.INT}
	    )
	  (* Uminus is just 0-num *)
	  | trexp (A.OpExp{left, oper=A.MinusOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=T.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.DivideOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=T.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.TimesOp, right, pos}) =
	    (checkInt(trexp left, pos);
	     checkInt(trexp right, pos);
	     {exp=(), ty=T.INT}
	    )
	  (*--Arithmetic--*)
	  (* Comparison -- int or string *)
	  | trexp (A.OpExp{left, oper=A.GtOp, right, pos}) =
	    (checkComp(trexp left, trexp right, pos);
	     {exp=(), ty=T.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.GeOp, right, pos}) =
	    (checkComp(trexp left, trexp right, pos);
	     {exp=(), ty=T.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.LtOp, right, pos}) =
	    (checkComp(trexp left, trexp right, pos);
	     {exp=(), ty=T.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.LeOp, right, pos}) =
	    (checkComp(trexp left, trexp right, pos);
	     {exp=(), ty=T.INT}
	    )
	  (* EqOp and NeqOp can take int, array, record on both sides *)
	  | trexp (A.OpExp{left, oper=A.EqOp, right, pos}) =
	    (checkEq(trexp left, trexp right, pos);
	     {exp=(), ty=T.INT}
	    )
	  | trexp (A.OpExp{left, oper=A.NeqOp, right, pos}) =
	    (checkEq(trexp left, trexp right, pos);
	     {exp=(), ty=T.INT}
	    )
	  (*--Comparison--*)
		
	  (* transDec = Let expressions *)
	  | trexp (A.LetExp{decList, body, pos}) =
	    let val {venv = venv', tenv = tenv'} = transDecs (venv, tenv, decList)
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
		
	  | trexp (A.RecordExp{fields = fList, typ = typ, pos = pos}) =
	    (let fun checkFields (fields, []) = ErrorMsg.error pos "extra parameters were defined for the record"
		   | checkFields ([], rFields) = ErrorMsg.error pos "not enough parameters were defined for the record"
		   | checkFields ([], []) = ()
		   | checkFields ((symb, exp, pos) :: list, rFields) =
		     let val matchingFields = List.filter (fn (sy, ty) => S.name(sy) = S.name(symb)) rFields
		     in
			 case matchingFields of [] => ErrorMsg.error pos ("expression parameter " ^ S.name(symb) ^ " does not match a record parameter")
					      | [(sy, ty)] => (if isSameType(tenv, pos, #ty(trexp(exp)), typeHelper(tenv, ty, pos))
							       then ()
							       else ErrorMsg.error pos("record types of " ^ getType(#ty(trexp(exp))) ^ " and " ^ getType(ty));
							       checkFields(list, List.filter (fn (sy, ty) => S.name(sy) <> S.name(symb)) rFields))
					      | _ => ErrorMsg.error pos "multiple paramters matched?"
		     end

		 val symbTy = if isSome(S.look(tenv, typ))
			      then valOf(S.look(tenv, typ))
			      else (ErrorMsg.error pos "record has not been defined";
				    T.NAME(typ, ref NONE))
		 val T.NAME(sym, ref) = case symbTy of T.NAME(s, r) => symbTy
						     | T.STRING => T.NAME(S.symbol(""), ref NONE)
						     | T.INT => T.NAME(S.symbol(""), ref NONE)
						     | _  => (ErrorMsg.error pos "??"; T.NAME(S.symbol(""), ref NONE))
		 val actualType = if isSome(!ref)
				  then typeHelper(tenv, valOf(!ref), pos)
				  else (ErrorMsg.error pos "expression does not have type of record";
					T.INT)
	     in
		 case actualType of T.RECORD(list, unique) => checkFields(fields, list)
				  | _ => ErrorMsg.error pos "record type is undefined";
		 {exp = (), ty = getActualType(tenv, typ, pos)}
	     end
	    )
		
	  | trexp (A.ArrayExp{typ = typ, size = size, init = init, pos = pos}) =
	    (checkInt(trexp size, pos);
	     if isSameType(tenv, pos, #ty (trexp(init)), typeHelper(tenv, arrayType(getActualType(tenv, typ, pos), pos), pos))
	     then ()
	     else ErrorMsg.error pos "type of initial value and array do not match";
	     {exp = (), ty = getActualType(tenv, typ, pos)})
		
	and trvar (A.FieldVar(var, sym, pos)) =
	    let val recordType = typeHelper(tenv, #ty(trvar(var)), pos)
	    in
		case recordType of T.RECORD(list, unique) =>
				   let val field = List.filter(fn (sy, ty) => S.name(sy) = S.name(sym)) list
				   in
				       case field of [(sy, ty)] => {exp = (), ty = typeHelper(tenv, ty, pos)}
						   | [] => (ErrorMsg.error pos ("record parameter " ^ S.name(sym) ^ " not found");
							    {exp = (), ty = T.INT})
						   | _  => (ErrorMsg.error pos ("record parameter " ^ S.name(sym) ^ " has multiple matches??");
							    {exp = (), ty = T.INT})
				   end
				 | _ => (ErrorMsg.error pos "variable is not a record"; {exp = (), ty = T.INT}) 
	    end
		
	  | trvar (A.SubscriptVar(var, exp, pos)) =
	    let val {exp = _, ty = varType} = trvar(var)
	    in
		checkInt(trexp(exp), pos);
		{exp = (), ty = typeHelper(tenv, arrayType(typeHelper(tenv, varType, pos), pos), pos)}
	    end
		
	  | trvar (A.SimpleVar(id, pos)) =
	    (case Symbol.look(venv, id)
	      of SOME (E.VarEntry{ty}) => {exp = (), ty = typeHelper(tenv, ty, pos)}
	       | SOME(E.FunEntry{...}) => (ErrorMsg.error pos ("improper function call or undefined variable" ^ S.name id); {exp = (), ty = T.INT})
	       | NONE => (ErrorMsg.error pos ("undefined variable " ^ S.name id);
			  exp=(), ty=T.INT))
    in
	trexp
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

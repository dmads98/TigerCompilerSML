structure A = Absyn
structure S = Symbol
structure E = Env
structure T = Types
structure F = FindEscape
structure Tr = Translate
		  
structure Semant : SEMANT =
struct

type venv = E.enventry S.table
type tenv = E.ty S.table
type ty = T.ty
type expty = {exp: Translate.exp, ty: ty}
val loopLevel = ref 0



(* Can use below for T.INT, T.STRING as is: Need to add more types *)
(* fun typeCheck (exp_ty, given_ty, pos) = *)
(*     if exp_ty <> given_ty then error pos "expected " ^ exp_ty ^ " saw " ^ given_ty else (); *)

fun typeHelper (tenv, T.NAME(symb, x_ref), pos) =
    let fun checkHelper (SOME typ) = typeHelper(tenv, typ, pos)
	  | checkHelper (NONE) = (ErrorMsg.error pos ("type " ^ (S.name symb) ^ " not yet defined"); T.INT)
    in
	checkHelper(!x_ref)
    end
  | typeHelper (tenv, typ: ty, pos) = typ 

fun getActualType (tenv, symb : S.symbol, pos) =
    let val value = S.look(tenv, symb)
    in
	case value of NONE => (ErrorMsg.error pos ("type " ^ (S.name symb) ^ " not yet defined"); T.INT)
		    | SOME(typ) => typeHelper(tenv, typ, pos)
    end;

fun arrayType (T.ARRAY(typ, x_ref), pos) = typ
  | arrayType (_, pos) = (ErrorMsg.error pos "expression does not have type of array";
			  T.INT);

fun isSameType  (tenv, pos, T.NIL, T.RECORD (typ, x_ref)) = true
  | isSameType (tenv, pos, T.RECORD (typ, x_ref), T.NIL) = true
  | isSameType (tenv, pos, T.RECORD (typ1, ref1), T.RECORD (typ2, ref2)) = ref1 = ref2
  | isSameType (tenv, pos, T.ARRAY(typ1, ref1), T.ARRAY(typ2, ref2)) = (ref1 = ref2) andalso
								       (isSameType(tenv, pos, typeHelper(tenv, typ1,pos), typeHelper(tenv, typ2, pos)))
  | isSameType (tenv, pos, typ1:ty, typ2:ty) = (typeHelper(tenv, typ1, pos) = typeHelper(tenv, typ2, pos))
						   
(* An exp is {exp=, ty=} *)
(* fun checkArith (exp1, exp2, pos) = *)
(*     ((typeCheck (#ty exp1, T.INT, pos); typeCheck (#ty exp2, T.INT, pos))); *)
fun getType (T.NIL) = "NIL"
  | getType (T.STRING) = "STRING"
  | getType (T.INT) = "INT"
  | getType (T.UNIT) = "UNIT"
  | getType (T.RECORD(ls, un)) = "RECORD"
  | getType (T.NAME(sym, ty)) = "NAME"
  | getType (T.ARRAY(ty, un)) = "ARRAY OF " ^ getType(ty)

fun member (x : S.symbol, nil) = false
  | member (x, y::ys) = x=y orelse member (x,ys); 


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
    then (ErrorMsg.error pos "expected matching record types to check for equality"; ())
    else ()
  | checkEq ({exp = exp1, ty = T.ARRAY(_, ref1)}, {exp = exp2, ty = T.ARRAY(_, ref2)}, pos) =
    if ref1 <> ref2
    then (ErrorMsg.error pos "expected matching array types to check for equality"; ())
    else ()   
  | checkEq ({exp = exp1, ty = _}, {exp = exp2, ty = _}, pos) = ErrorMsg.error pos "expected matching types to check for equality";

fun cycleExists (tenv, symb, list) =
    case S.look(tenv, symb)
     of SOME(T.NAME(_, x_ref)) =>
	(case !x_ref of
	     SOME(T.NAME(sym, _)) => if member(sym, list)
				     then true
				     else cycleExists(tenv, sym, list @ [symb])
	   | _ => false)
      | SOME(_) => false
      | NONE => false

fun transExp (venv, tenv, level, doneLabel) =
    let fun trexp (A.NilExp) = {exp = Tr.nilExp(), ty = T.NIL}
	  | trexp (A.IntExp(int)) = {exp = Tr.intExp(int), ty = T.INT}
	  | trexp (A.StringExp(str, pos)) = {exp = Tr.stringExp(str), ty = T.STRING}
	  | trexp (A.VarExp(var)) = trvar(var)
	  | trexp (A.AssignExp{var, exp, pos}) =
	    let val {exp = varExpr, ty = varType} = trvar(var)
		val {exp = expExpr, ty = expType} = trexp(exp)
	    in
		if isSameType(tenv, pos, typeHelper(tenv, varType, pos),
			   typeHelper(tenv, expType, pos))
		then {exp = Tr.assignExp(varExpr, expExpr), ty = T.UNIT}
		else (ErrorMsg.error pos "variable and expression types do not match";
		      {exp = Tr.nilExp(), ty = T.UNIT})
	     end	
	  | trexp (A.IfExp{test, then', else', pos}) =
	    let val {exp = testExp, ty = testTy} = trexp test
	    in
		(checkInt({exp = testExp, ty = testTy}, pos);
		 if isSome(else')
		 then
		     let val {exp = thenExp, ty = thenTy} = trexp then'
			 val {exp = elseExp, ty = elseTy} = trexp (valOf(else'))
		     in
			 (if isSameType(tenv, pos, typeHelper(tenv, thenTy, pos), typeHelper(tenv, elseTy, pos))
			  then ()
			  else ErrorMsg.error pos "then and else statements return types do not match";
			  {exp = Tr.ifElse(testExp, thenExp, elseExp), ty = typeHelper(tenv, thenTy, pos)})
		     end
		 else
		     let val {exp = thenExp, ty = thenTy} = trexp then'
		     in
			 (if isSameType(tenv, pos, typeHelper(tenv, #ty(trexp then'), pos), T.UNIT)
			  then ()
			  else ErrorMsg.error pos "then statement does not have type UNIT";
			  {exp = Tr.ifThen(testExp, thenExp), ty = T.UNIT})
		     end
		)
	    end

	  | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
	    let val {exp = expL, ty = tyL} = trexp lo
		val {exp = expH, ty = tyH} = trexp hi
		val breakLab = (loopLevel := !loopLevel + 1;
				Temp.newlabel())
	    in
		(checkInt({exp = expL, ty = tyL}, pos);
		 checkInt({exp = expR, ty = tyR}, pos);
		 loopLevel := !loopLevel + 1;
		 let val tempVenv = S.enter(venv, var, E.VarEntry{access = Tr.allocLocal(level)(!escape), ty = T.INT})
		     val {exp = expB, ty = tyB} = transExp(tempVenv, tenv, level, SOME(breakLab)) body
		 in
		     if isSameType(tenv, pos, tyB, T.UNIT)
		     then (loopLevel := !loopLevel - 1;
			   {exp = Tr.forExp(expL, expH, expB, breakLab), ty = T.UNIT})
		     else (loopLevel := !loopLevel - 1;
			   ErrorMsg.error pos "body of for loop should have type UNIT";
			   {exp = Tr.nilExp(), ty = T.INT})
		 end
		)
	    end

	  | trexp (A.WhileExp{test, body, pos}) =
	    let val breakLab = (loopLevel := !loopLevel + 1;
				Temp.newlabel())
		val {exp = expB, ty = tyB} = transExp(venv, tenv, level, SOME(breakLab)) body
		val {exp = expT, ty = tyT} = transExp(venv, tenv, level, doneLabel) test
	    in
		(checkInt({exp = expT, ty = tyT}, pos);
		 if isSameType(tenv, pos, tyB, T.UNIT)
		 then (loopLevel := !loopLevel - 1;
		       {exp = Tr.whileExp(expT, expB, breakLab, ty = T.UNIT})
		 else (loopLevel := !loopLevel - 1;
		       ErrorMsg.error pos "body of while loop should have type UNIT";
		       {exp = Tr.nilExp(), ty = T.INT})
		)
	    end
		
	  | trexp (A.BreakExp(pos)) =
	    (if (!loopLevel < 0)
	     then ErrorMsg.error pos "not possible??"
	     else ();
	     if (!loopLevel = 0)
	     then (ErrorMsg.error pos "break expression exists outside of a loop";
		   {exp = Tr.nilExp(), ty = T.UNIT})
	     else {exp = Tr.breakExp(doneLabel), ty = T.UNIT}
	    )

	  | trexp (A.CallExp{func, args, pos}) = 
	    let val exptys = map (fn e => trexp e) args
		fun checkParamTypes (arg, index) =
		    (if isSameType(tenv, pos, typeHelper(tenv, arg, pos), typeHelper(tenv, #ty(List.nth(exptys, index)), pos))
		     then ()
		     else ErrorMsg.error pos "parameter types do not match";
		     index + 1)
		val E.FunEntry{level = fLevel, label = fLabel, formals, result} = case S.look(venv, func) of SOME(E.VarEntry{access, ty}) => ((ErrorMsg.error pos "identifier is a variable not a function"); E.FunEntry{level = level, label = Temp.newlabel(), formals = [], result = T.UNIT})
								      | SOME(E.FunEntry{level = fLevel, label = fLabel, formals, result}) => valOf(S.look(venv, func))
								      | _ => ((ErrorMsg.error pos "function is undefined"); E.FunEntry{level = level, label = Temp.newlabel(), formals = [], result = T.UNIT})
	    in
		if List.length(formals) <> List.length(args)
		then (ErrorMsg.error pos "number of arguments do not match"; ())
		else (foldl checkParamTypes 0 formals; ());
		{exp = Tr.callExp(level, fLevel, fLabel, map (fn e => #exp(e)) exptys), ty = typeHelper(tenv, result, pos)}
	    end
		
	  (* Arithmetic *)
	  | trexp (A.OpExp{left, oper = A.PlusOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkInt({exp = expL, ty = tyL}, pos);
		 checkInt({exp = expR, ty = tyR}, pos);
		 {exp= Tr.binop(Tree.PLUS, expL, expR), ty=T.INT}
		)
	    end
	  (* Uminus is just 0-num *)
	  | trexp (A.OpExp{left, oper=A.MinusOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkInt({exp = expL, ty = tyL}, pos);
		 checkInt({exp = expR, ty = tyR}, pos);
		 {exp= Tr.binop(Tree.MINUS, expL, expR), ty=T.INT}
		)
	    end
	  | trexp (A.OpExp{left, oper=A.DivideOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkInt({exp = expL, ty = tyL}, pos);
		 checkInt({exp = expR, ty = tyR}, pos);
		 {exp= Tr.binop(Tree.DIV, expL, expR), ty=T.INT}
		)
	    end
	  | trexp (A.OpExp{left, oper=A.TimesOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkInt({exp = expL, ty = tyL}, pos);
		 checkInt({exp = expR, ty = tyR}, pos);
		 {exp= Tr.binop(Tree.MUL, expL, expR), ty=T.INT}
		)
	    end
	  (*--Arithmetic--*)
	  (* Comparison -- int or string *)
	  | trexp (A.OpExp{left, oper=A.GtOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkComp({exp = expL, ty = tyL}, {exp = expR, ty = tyR}, pos);
		 {exp= Tr.relop(Tree.GT, expL, expR, tyL), ty=T.INT}
		)
	    end
	  | trexp (A.OpExp{left, oper=A.GeOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkComp({exp = expL, ty = tyL}, {exp = expR, ty = tyR}, pos);
		 {exp= Tr.relop(Tree.GE, expL, expR, tyL), ty=T.INT}
		)
	    end
	  | trexp (A.OpExp{left, oper=A.LtOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkComp({exp = expL, ty = tyL}, {exp = expR, ty = tyR}, pos);
		 {exp= Tr.relop(Tree.LT, expL, expR, tyL), ty=T.INT}
		)
	    end
	  | trexp (A.OpExp{left, oper=A.LeOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkComp({exp = expL, ty = tyL}, {exp = expR, ty = tyR}, pos);
		 {exp= Tr.relop(Tree.LE, expL, expR, tyL), ty=T.INT}
		)
	    end
	  (* EqOp and NeqOp can take int, array, record on both sides *)
	  | trexp (A.OpExp{left, oper=A.EqOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkEq({exp = expL, ty = tyL}, {exp = expR, ty = tyR}, pos);
		 {exp= Tr.relop(Tree.EQ, expL, expR, tyL), ty=T.INT}
		)
	    end
	  | trexp (A.OpExp{left, oper=A.NeqOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkEq({exp = expL, ty = tyL}, {exp = expR, ty = tyR}, pos);
		 {exp= Tr.relop(Tree.NE, expL, expR, tyL), ty=T.INT}
		)
	    end
	  (*--Comparison--*)
		
	  (* transDec = Let expressions *)
	  | trexp (A.LetExp{decs = decList, body = body, pos = pos}) =
	    let val {venv = venv', tenv = tenv', expList = expList'} = transDecs (venv, tenv, decList, level, doneLabel)
		val {exp = exp', ty = ty'} = transExp(venv', tenv', level, doneLabel)
	    in
		{exp = Tr.letBody(expList', exp'), ty = ty'}
	    end
		
	  | trexp (A.SeqExp (exprList)) =
	    let val exptyList = map (fn (expr, pos) => trexp(expr)) exprList
		val seqType = if List.length(exptyList) = 0
			      then T.UNIT
			      else #ty(List.last(exptyList))
		val expList = map (fn e => #exp(e)) exptyList
	    in
		{exp = Tr.seqExp(expList), ty = seqType}
	    end
			
	  | trexp (A.RecordExp{fields = fList, typ = typ, pos = pos}) =
	    let fun checkFields ([], []) = ()
		  | checkFields (fields, []) = ErrorMsg.error pos "extra parameters were defined for the record"
		  | checkFields ([], rFields) = ErrorMsg.error pos "not enough parameters were defined for the record"
		  | checkFields ((symb, {exp = fExp, ty = fTy}, pos)::list, (rSym, rTy)::rFields) =
		    if S.name(symb) = S.name(rSym) andalso isSameType(tenv, pos, typeHelper(tenv, fTy, pos), typeHelper(tenv, rTy, pos))
		    then checkFields(list, rFields)
		    else ErrorMsg.error pos("record fields do not match " ^ S.name(symb) ^ " and " ^ S.name(rSym))
					
		val symbTy = if isSome(S.look(tenv, typ))
			     then valOf(S.look(tenv, typ))
			     else (ErrorMsg.error pos "record has not been defined";
				   T.NAME(typ, ref NONE))
		val T.NAME(sym, x_ref) = case symbTy of T.NAME(s, r) => symbTy
						      | T.STRING => T.NAME(S.symbol(""), ref NONE)
						      | T.INT => T.NAME(S.symbol(""), ref NONE)
						      | _  => (ErrorMsg.error pos "??"; T.NAME(S.symbol(""), ref NONE))
		val actualType = if isSome(!x_ref)
				 then typeHelper(tenv, valOf(!x_ref), pos)
				 else (ErrorMsg.error pos "expression does not have type of record";
				       T.INT)
		val fList' = map (fn (sy, ex, pos) => (sy, (trexp ex), pos)) fList
	    in
		(case actualType
		  of T.RECORD(list, unique) => checkFields(fList', list)
		   | _ => ErrorMsg.error pos "record type is undefined";
		 {exp = Tr.recordExp(map (fn (sy, expty, pos) => #exp(expty)) fList'), ty = getActualType(tenv, typ, pos)})
	    end

	  | trexp (A.ArrayExp{typ = typ, size = size, init = init, pos = pos}) =
	    let val {exp = szExp, ty = szTy} = trexp(size)
		val {exp = inExp, ty = inTy} = trexp(init)
	    in
		(checkInt({exp = szExp, ty = szTy}, pos);
		 if isSameType(tenv, pos, inTy), typeHelper(tenv, arrayType(getActualType(tenv, typ, pos), pos), pos))
		 then ()
		 else ErrorMsg.error pos "type of initial value and array do not match";
		 {exp = Tr.arrExp(szExp, inExp), ty = getActualType(tenv, typ, pos)})
	    end
		    
	and trvar (A.FieldVar(var, sym, pos)) =
	    let val {exp = vExp, ty = vTy} = trvar(var)
		val recordType = typeHelper(tenv, vTy, pos)
	    in case recordType
		of T.RECORD(list, unique) =>
		   let val field = List.filter(fn (sy, ty) => S.name(sy) = S.name(sym)) list
		       val (curIndex, ansIndex) =
			   let fun findField (curDex, ansDex) =
				   let val (sy, ty) = List.nth(list, curDex)
				   in
				       if S.name(sy) = S.name(sym)
				       then (curDex + 1, curDex)
				       else (curDex + 1, ansDex)
				   end
			   in
			       foldl findField(0, ~1) list
			   end  
		   in
		       case field of [(sy, ty)] => {exp = Tr.fieldVar(vExp, ansIndex), ty = typeHelper(tenv, ty, pos)}
				   | [] => (ErrorMsg.error pos ("record parameter " ^ S.name(sym) ^ " not found");
					    {exp = Tr.nilExp(), ty = T.INT})
				   | _  => (ErrorMsg.error pos ("record parameter " ^ S.name(sym) ^ " has multiple matches??");
					    {exp = Tr.nilExp(), ty = T.INT})
		   end
		 | _ => (ErrorMsg.error pos "variable is not a record"; {exp = Tr.nilExp(), ty = T.INT}) 
	    end
		
	  | trvar (A.SubscriptVar(var, exp, pos)) =
	    let val dex = trexp(exp)
		val {exp = varExpr, ty = varType} = trvar(var)
	    in
		checkInt(dex, pos);
		{exp = Tr.subscriptExp(varExpr, #exp(index)), ty = typeHelper(tenv, arrayType(typeHelper(tenv, varType, pos), pos), pos)}
	    end	
	  | trvar (A.SimpleVar(id, pos)) =
	    (case S.look(venv, id)
	      of SOME (E.VarEntry{access, ty}) => {exp = Tr.simpleVar(access, level), ty = typeHelper(tenv, ty, pos)}
	       | SOME(E.FunEntry{...}) => (ErrorMsg.error pos ("improper function call or undefined variable" ^ S.name id); {exp = Tr.nilExp(), ty = T.INT})
	       | NONE => (ErrorMsg.error pos ("undefined variable " ^ S.name id);
			  {exp = Tr.nilExp(), ty=T.INT}))
    in
	trexp
    end
	
and transDecs (venv, tenv, [], level, doneLabel) = {venv = venv, tenv = tenv, expList = []}
  | transDecs (venv, tenv, decs, level, doneLabel)  = 
    let fun trdec (A.VarDec{name, escape, typ = SOME((assignType, typPos)), init, pos}, {venv, tenv, expList}) =
	    let val {exp, ty = typeFound} = transExp(venv, tenv, level, doneLabel) init
		val varAccess = Tr.allocLocal(level)(!escape)
	    in
		if isSameType(tenv, pos, getActualType(tenv, assignType, typPos), typeHelper(tenv, typeFound, pos))
		then {venv = S.enter(venv, name, E.VarEntry {access = varAccess, ty = typeFound}), tenv = tenv, expList = expList @ [Tr.varDec(access, exp)]}
		else (ErrorMsg.error pos ("body of var declaration has incorrect type");
		      {venv = venv, tenv = tenv, expList = expList})
	    end
	  | trdec (A.VarDec{name, escape, typ = NONE, init, pos}, {venv, tenv, expList}) =
	    let val {exp, ty = typeFound} = transExp(venv, tenv, level, doneLabel) init
		val varAccess = Tr.allocLocal(level)(!escape)
	    in
		if typeFound <> T.NIL
		then {venv = S.enter(venv, name, E.VarEntry {access = varAccess, ty = typeFound}), tenv = tenv, expList = expList @ [Tr.varDec(varAccess, exp)]}
		else (ErrorMsg.error pos ("cannot assign var to nil for a non-record type");
		      {venv = venv, tenv = tenv, expList = expList})
	    end

	  | trdec (A.FunctionDec(decList), {venv, tenv, expList}) =
	    let
		fun createTempVenv (pairList, venv) =
		    let fun insertIntoVenv((access, {name, escape, typ, pos}), venv) = (S.enter(venv, name, E.VarEntry{access = access, ty = getActualType(tenv, typ, pos)}))
		    in
			foldl insertIntoVenv venv params
		    end
			
		fun checkFuncDec (venv, tenv, {name, params, result = NONE, body, pos}) =
		    let val entry = S.look(venv, name)
			val entryLevel = case entry of SOME(E.FunEntry{level, label, formals, result}) => level
						     | _ => (ErrorMsg.error ~1 "function is not in venv"; Tr.outermost)
			val (statLink::formals) = Tr.formals(entryLevel)
			val pairList = ListPair.zip(formals, params)			
			val tempVenv = createTempVenv(pairList, venv)
			val {exp = exp', ty = ty'} = transExp(tempVenv, tenv, entryLevel, NONE) body
		    in
			if isSameType(tenv, pos, ty', T.UNIT)
			then (T.procEntryExit({level = entryLevel, body = exp'});
			      {venv = venv, tenv = tenv, expList = expList})
			else (ErrorMsg.error pos ("function body and return type do not match"); {venv = venv, tenv = tenv, expList = expList})
		    end
		  | checkFuncDec (venv, tenv, {name, params, result = SOME(retType, pos2), body, pos}) =
		    let val entry = S.look(venv, name)
			val entryLevel = case entry of SOME(E.FunEntry{level, label, formals, result}) => level
						     | _ => (ErrorMsg.error ~1 "function is not in venv"; Tr.outermost)
			val (statLink::formals) = Tr.formals(entryLevel)
			val pairList = ListPair.zip(formals, params)			
			val tempVenv = createTempVenv(pairList, venv)
			val {exp = exp', ty = ty'} = transExp(tempVenv, tenv, entryLevel, NONE) body
		    in
			if isSameType(tenv, pos, ty', getActualType(tenv, retType, pos))
			then (T.procEntryExit({level = entryLevel, body = exp'});
			      {venv = venv, tenv = tenv, expList = expList})
			else (ErrorMsg.error pos ("function body and return type do not match"); {venv = venv, tenv = tenv, expList = expList})
		    end

		fun updateVenv ({name, params, result = NONE, body, pos}, venv, tenv) =
		    let val lName = Temp.newlabel()
			val nextLevel = Tr.newLevel({parent = level, name = lName, formals = map(fn {name, escape, typ, pos} => !escape) params})
			val paramTypes = map (fn {name, escape, typ = symb, pos} => getActualType(tenv, symb, pos)) params
		    in
			{venv = S.enter(venv, name, E.FunEntry{level = nextLevel, label = lName, formals = paramTypes, result = T.UNIT}), tenv = tenv, expList = expList}
		    end
		  | updateVenv({name, params, result = SOME(sym, pos2), body, pos}, venv, tenv) =
		    let val lName = Temp.newlabel()
			val nextLevel = Tr.newLevel({parent = level, name = lName, formals = map(fn {name, escape, typ, pos} => !escape) params})
			val paramTypes = map (fn {name, escape, typ = symb, pos} => getActualType(tenv, symb, pos)) params
		    in
			{venv = S.enter(venv, name, E.FunEntry{level = nextLevel, label = lName, formals = paramTypes, result = getActualType(tenv, sym, pos)}), tenv = tenv, expList = expList}
		    end 

		fun handleFuncNames (venv, tenv, ls, []) = {venv = venv, tenv = tenv, expList = expList}
		  | handleFuncNames (venv, tenv, ls, [fDec]) =
		    if member (#name(fDec), ls)
		    then (ErrorMsg.error (#pos(fDec)) ("function " ^ S.name(#name(fDec)) ^ " already declared in block"); {tenv = tenv, venv = venv, expList = expList})
		    else updateVenv(fDec, venv, tenv)
		  | handleFuncNames (venv, tenv, ls, fDec::decs) =
		    let val {venv = venv', tenv = tenv', expList = expList'} =
			    if member(#name(fDec), ls)
			    then (ErrorMsg.error (#pos(fDec)) ("function " ^ S.name(#name(fDec)) ^ " already declared in block"); {tenv = tenv, venv = venv, expList = expList})
			    else updateVenv(fDec, venv, tenv)
		    in
			handleFuncNames(venv', tenv', ls @ [#name(fDec)], decs)
		    end

		fun handleFuncDecs (venv, tenv, []) = {venv = venv, tenv = tenv, expList = expList}
		  | handleFuncDecs (venv, tenv, [fDec]) = checkFuncDec(venv, tenv, fDec)
		  | handleFuncDecs (venv, tenv, fDec::list) =
		    let val {venv = venv', tenv = tenv', expList = expList' } = checkFuncDec(venv, tenv, fDec)
		    in
			handleFuncDecs(venv', tenv', list)
		    end
			
	    in
		let val {venv = venv', tenv = tenv', expList = explist'} = handleFuncNames(venv, tenv, [], decList)
		in
		    handleFuncDecs(venv', tenv', decList)
		end
	    end
	  | trdec (A.TypeDec(decList), {venv, tenv, expList}) =
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
			setRef(typeOption, valOf(nameVal));
			{venv = venv, tenv = tenv, expList = expList}
		    end
		  | checkTypeDec (venv, tenv, {name, ty = A.NameTy(symb, tyPos), pos})  = 
		    let val symbVal = S.look(tenv, symb)
			val nameVal = S.look(tenv, name)
			val actualType = if isSome(symbVal)
					 then symbVal
					 else (ErrorMsg.error pos ("type " ^ S.name symb ^ " not yet declared"); NONE)
			fun setRef(newRef, T.NAME(name, oldRef)) = ((oldRef := newRef); ())
			  | setRef (_) = () 
		    in
			setRef(actualType, valOf(nameVal));

			if cycleExists (tenv, name, [])
			then (ErrorMsg.error pos "declared type causes a cycle"; setRef(NONE, valOf(nameVal)))
			else ();
			{venv = venv, tenv = tenv, expList = expList}
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
			val (_, fields) = foldl checkFields ([],[]) fields
			val typeOption = SOME(T.RECORD(fields, ref ()))
			fun setRef(newRef, T.NAME(name, oldRef)) = ((oldRef := newRef); ())
			  | setRef (_) = () 
		    in
			setRef(typeOption, valOf(nameVal));
			{venv = venv, tenv = tenv, expList = expList}
		    end

			
		fun handleTypeNames (venv, tenv, ls, []) = {venv = venv, tenv = tenv, expList = expist}
		  | handleTypeNames (venv, tenv, ls, [{name, ty, pos}]) =
		    if member (name, ls)
		    then (ErrorMsg.error pos ("type " ^ S.name(name) ^ " already declared in block"); {tenv = tenv, venv = venv, expList = expList})
		    else {venv = venv, tenv = S.enter(tenv, name, T.NAME(name, (ref NONE))), expList = expList}
		  | handleTypeNames (venv, tenv, ls, tDec::decs) =
		    let val {venv = venv', tenv = tenv', expList = expList'} =
			    if member (#name(tDec), ls)
			    then (ErrorMsg.error (#pos(tDec)) ("type " ^ S.name(#name(tDec)) ^ " already declared in block"); {tenv = tenv, venv = venv, expList = expList})
			    else {venv = venv, tenv = S.enter(tenv, #name(tDec), T.NAME(#name(tDec), (ref NONE))), expList = expList}
		    in
			handleTypeNames(venv', tenv', ls @ [#name(tDec)], decs)
		    end
		fun handleTypeDecs (venv, tenv, []) = {venv = venv, tenv = tenv, expList = expList}
		  | handleTypeDecs (venv, tenv, [tDec]) = checkTypeDec(venv, tenv, tDec)
		  | handleTypeDecs (venv, tenv, tDec::list) =
		    let val {venv = venv', tenv = tenv', expList = expList'} = checkTypeDec(venv, tenv, tDec)
		    in
			handleTypeDecs(venv', tenv', list)
		    end
	    in
		let val {venv = venv', tenv = tenv', expList = expList'} = handleTypeNames(venv, tenv, [], decList)
		in
		    handleTypeDecs(venv', tenv', decList)
		end
	    end
    in
	foldl trdec {venv = venv, tenv = tenv, expList = []} decs 
    end

fun transProg(exp) =
    let val startLevel = Tr.newLevel({parent = Tr.outermost, name = Temp.newlabel(), formals = []})
	val _ = F.findEscape(exp)
	val result = #exp(transExp(E.base_venv, E.base_tenv, exp, startLevel, NONE))
	val _ = Tr.procEntryExit({level = startLevel, body = result})
	val frags = Tr.getResult()
    in
	frags
    end

end

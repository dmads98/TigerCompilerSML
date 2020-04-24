structure Semant : SEMANT =
struct

structure A = Absyn
structure S = Symbol
structure E = Env
structure T = Types
structure F = FindEscape
structure Tr = Translate

type venv = E.enventry S.table
type tenv = E.ty S.table
type ty = T.ty
type expty = {exp: Translate.exp, ty: ty}
val loopLevel = ref 0
		    
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
  | getType _  = "BOTTOM"


fun checkInt ({exp, ty}, pos) = case ty of T.INT => ()
					 | ty => ErrorMsg.error pos ("integer expected, found " ^ getType(ty)); 

(* Both int or both string is okay *)
fun checkComp ({exp = exp1, ty = T.INT}, {exp = exp2, ty = T.INT}, pos) = ()
  | checkComp ({exp = exp1, ty = _}, {exp = exp2, ty = _}, pos) = ErrorMsg.error pos "expected a matching int for comparison";

(* Both int or string or array or record refs is okay *)
fun checkEq ({exp = exp1, ty = T.INT}, {exp = exp2, ty = T.INT}, pos) = ()
  | checkEq ({exp = exp1, ty = T.STRING}, {exp = exp2, ty = T.STRING}, pos) = ()
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

fun canAssign (var, varVal, pos, msg) = if T.getComp(var, varVal) = T.EQ orelse T.getComp(var, varVal) = T.GT
					then ()
					else ErrorMsg.error pos msg

fun equalTypes (ty1, ty2, pos, err) = if T.getEQ(ty1, ty2)
				      then ()
				      else ErrorMsg.error pos err

fun inLoop (pos, err) = if !loopLevel = 0
			then ErrorMsg.error pos err
			else ()

fun transExp (venv, tenv, exp, level : Tr.level, doneLabel) : expty =
    let fun trexp (A.NilExp) = {exp = Tr.transNIL, ty = T.NIL}
	  | trexp (A.IntExp(x)) = {exp = Tr.transINT(x), ty = T.INT}
	  | trexp (A.StringExp(str, pos)) = {exp = Tr.transSTRING(str), ty = T.STRING}
	  | trexp (A.VarExp(var)) = trvar(var)
	  | trexp (A.AssignExp{var, exp, pos}) =
	    let fun getVar v = case v of A.SimpleVar(s, _) => S.look(venv, s)
				       | A.FieldVar(v, _, _) => getVar v
				       | A.SubscriptVar(v, _, _) => getVar v
		fun isAssignable v = case getVar v of SOME(E.VarEntry({access, ty, read})) => if read
											      then ErrorMsg.error pos "read only variable error"
											      else ()
						    | _  => ErrorMsg.error pos "unable to assign"
		val {exp = varExpr, ty = varType} = trvar(var)
		val {exp = expExpr, ty = expType} = trexp(exp)
	    in
		isAssignable var;
		canAssign(varType, expType, pos, "assign types do not match");
		{exp = Tr.transASSIGN(varExpr, expExpr), ty = T.UNIT}
	    end	
	  | trexp (A.IfExp{test, then', else', pos}) =
	    let val {exp = testExp, ty = testTy} = trexp test
	    in
		(equalTypes(testTy, T.INT, pos, "if exp test is not an int");
		 if isSome(else')
		 then
		     let val {exp = thenExp, ty = thenTy} = trexp then'
			 val {exp = elseExp, ty = elseTy} = trexp (valOf(else'))
		     in
			 (case (thenTy, elseTy) of (T.RECORD _, NIL) => {exp = Tr.transIFELSE(testExp, thenExp, elseExp), ty = thenTy}
						 | (NIL, T.RECORD _) => {exp = Tr.transIFELSE(testExp, thenExp, elseExp), ty = elseTy}
						 | (ty1, ty2) => (equalTypes(ty1, ty2, pos, "then and else types do not match");
								  {exp = Tr.transIFELSE(testExp, thenExp, elseExp), ty = thenTy}))
		     end
		 else
		     let val {exp = thenExp, ty = thenTy} = trexp then'
		     in
			 (equalTypes(thenTy, T.UNIT, pos, "then clause is not a unit");
			  {exp = Tr.transIFTHEN(testExp, thenExp), ty = thenTy})
		     end
		)
	    end

	  | trexp (A.ForExp{var, escape, lo, hi, body, pos}) =
	    let val {exp = expL, ty = tyL} = trexp lo
		val {exp = expH, ty = tyH} = trexp hi
		val breakLab = (loopLevel := !loopLevel + 1;
				Temp.newlabel())
		val tempVenv = S.enter(venv, var, E.VarEntry({access = Tr.allocLocal level true, ty = T.INT, read = true}))
				      
	    in
		(equalTypes(tyL, T.INT, pos, "for exp lo is not int");
		 equalTypes(tyH, T.INT, pos, "for exp hi is not int");
		 let val {exp = expB, ty = tyB} = transExp(tempVenv, tenv, body, level, breakLab)
		     val _ = equalTypes(tyB, T.UNIT, pos, "for exp body is not unit")
		     val _ = loopLevel := !loopLevel - 1
		 in
		     case S.look(tempVenv, var) of
			 SOME(v) => (case v of E.VarEntry{access, ty, read} => {exp = Tr.transFOR(Tr.simpleVar(access, level), escape, expL, expH, expB, breakLab), ty = T.UNIT}
					     | _ => (ErrorMsg.error 0 "error related to for loop";
						     {exp = (Tr.transCONST 0), ty = T.UNIT}))
		       | _ => (ErrorMsg.error 0 "for loop var not found";
			       {exp = (Tr.transCONST 0), ty = T.UNIT})
				  
		 end
		)
	    end

	  | trexp (A.WhileExp{test, body, pos}) =
	    let	val {exp = expT, ty = tyT} = transExp(venv, tenv, test, level, doneLabel)
		val breakLab = (loopLevel := !loopLevel + 1;
				Temp.newlabel())
		val {exp = expB, ty = tyB} = transExp(venv, tenv, body, level, breakLab)
	    in
		(equalTypes(tyT, T.INT, pos, "while test is not an int");
		 equalTypes(tyB, T.UNIT, pos, "while body is not unit");
		 loopLevel := !loopLevel - 1;
		 {exp = Tr.transWHILE(expT, expB, breakLab), ty = T.UNIT})
	    end
		
	  | trexp (A.BreakExp(pos)) =
	    (inLoop(pos, "break exp not correct");
	     {exp = Tr.transBREAK(doneLabel), ty = T.UNIT})

	  | trexp (A.CallExp{func, args, pos}) = 
	    let val exptys = map (fn e => #exp(trexp e)) args
		fun checkParamTypes (fTy :: fList, a :: aList, pos) = if T.getEQ(fTy, #ty(trexp a))
								      then checkParamTypes(fList, aList, pos)
								      else ErrorMsg.error pos "types of formal and actual do not match"
		  | checkParamTypes ([], a::aList, pos) = ErrorMsg.error pos "extra parameters passed"
		  | checkParamTypes (fTy::fList, [], pos) = ErrorMsg.error pos "not enough parameters passed"
		  | checkParamTypes ([], [], pos) = ()
	    in
		case S.look(venv, func) of
		    SOME(E.FunEntry{level = fLevel, label = fLabel, formals, result}) => (checkParamTypes(formals, args, pos);
											  {exp = Tr.transCALL(fLevel, level, fLabel, exptys), ty=result})
		  | SOME(_) => (ErrorMsg.error pos ("symbol " ^ S.name(func) ^ " is not a function");
				{exp = (Tr.transCONST 0), ty = T.BOTTOM})
		  | NONE => (ErrorMsg.error pos ("function " ^ S.name(func) ^ " is not defined");
			     {exp = (Tr.transCONST 0), ty = T.BOTTOM})
	    end
		
	  (* Arithmetic *)
	  | trexp (A.OpExp{left, oper = A.PlusOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkInt({exp = expL, ty = tyL}, pos);
		 checkInt({exp = expR, ty = tyR}, pos);
		 {exp= Tr.transBINOP(expL, A.PlusOp, expR), ty=T.INT}
		)
	    end
	  (* Uminus is just 0-num *)
	  | trexp (A.OpExp{left, oper=A.MinusOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkInt({exp = expL, ty = tyL}, pos);
		 checkInt({exp = expR, ty = tyR}, pos);
		 {exp= Tr.transBINOP(expL, A.MinusOp, expR), ty=T.INT}
		)
	    end
	  | trexp (A.OpExp{left, oper=A.DivideOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkInt({exp = expL, ty = tyL}, pos);
		 checkInt({exp = expR, ty = tyR}, pos);
		 {exp= Tr.transBINOP(expL, A.DivideOp, expR), ty=T.INT}
		)
	    end
	  | trexp (A.OpExp{left, oper=A.TimesOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkInt({exp = expL, ty = tyL}, pos);
		 checkInt({exp = expR, ty = tyR}, pos);
		 {exp= Tr.transBINOP(expL, A.TimesOp, expR), ty=T.INT}
		)
	    end
	  (*--Arithmetic--*)
	  | trexp (A.OpExp{left, oper=A.GtOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkComp({exp = expL, ty = tyL}, {exp = expR, ty = tyR}, pos);
		 {exp= Tr.transRELOP(expL, A.GtOp, expR, tyL), ty=T.INT}
		)
	    end
	  | trexp (A.OpExp{left, oper=A.GeOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkComp({exp = expL, ty = tyL}, {exp = expR, ty = tyR}, pos);
		 {exp= Tr.transRELOP(expL, A.GeOp, expR, tyL), ty=T.INT}
		)
	    end
	  | trexp (A.OpExp{left, oper=A.LtOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkComp({exp = expL, ty = tyL}, {exp = expR, ty = tyR}, pos);
		 {exp= Tr.transRELOP(expL, A.LtOp, expR, tyL), ty=T.INT}
		)
	    end
	  | trexp (A.OpExp{left, oper=A.LeOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkComp({exp = expL, ty = tyL}, {exp = expR, ty = tyR}, pos);
		 {exp= Tr.transRELOP(expL, A.LeOp, expR, tyL), ty=T.INT}
		)
	    end
	  (* EqOp and NeqOp can take int, array, record on both sides *)
	  | trexp (A.OpExp{left, oper=A.EqOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkEq({exp = expL, ty = tyL}, {exp = expR, ty = tyR}, pos);
		 {exp= Tr.transRELOP(expL, A.EqOp, expR, tyL), ty=T.INT}
		)
	    end
	  | trexp (A.OpExp{left, oper=A.NeqOp, right, pos}) =
	    let val {exp = expL, ty = tyL} = trexp left
		val {exp = expR, ty = tyR} = trexp right
	    in
		(checkEq({exp = expL, ty = tyL}, {exp = expR, ty = tyR}, pos);
		 {exp= Tr.transRELOP(expL, A.NeqOp, expR, tyL), ty=T.INT}
		)
	    end
	  (*--Comparison--*)
		
	  (* transDec = Let expressions *)
	  | trexp (A.LetExp{decs = decList, body = body, pos = pos}) =
	    let val curLevel = !loopLevel
		val _ = loopLevel := 0
		val {venv = venv', tenv = tenv', expList = expList'} = transDecs (venv, tenv, decList, level, doneLabel)
		val _ = loopLevel := curLevel
		val {exp = exp', ty = ty'} = transExp(venv', tenv', body, level, doneLabel)
	    in
		{exp = Tr.transLET(expList', exp'), ty = ty'}
	    end
		
	  | trexp (A.SeqExp (exprList)) =
	    let val exptyList = map (fn (expr, pos) => trexp(expr)) exprList
		val seqType = if List.length(exptyList) = 0
			      then T.UNIT
			      else #ty(List.last(exptyList))
		val expList = map (fn e => #exp(e)) exptyList
	    in
		{exp = Tr.transSEQ(expList), ty = seqType}
	    end
		
	  | trexp (A.RecordExp{fields = fList, typ = typ, pos = pos}) =
	    (case S.look(tenv, typ) of
		 SOME(t) => (case t of T.RECORD(func, _) =>
				       (let val rFormals : (S.symbol * S.symbol) list = (func ())
					    fun fieldType (name:string, (s, e, pos) :: ls) =
						if String.compare(name, S.name(s)) = EQUAL
						then #ty(trexp e)
						else fieldType(name, ls)
					      | fieldType (name :string, []) = T.BOTTOM
					    fun tyCheckFormals (s, ty) =
						if (T.typeComp(fieldType(S.name(s), fList), ty))
						then ()
						else ErrorMsg.error pos ("record formal and actual type " ^ S.name(s) ^ " do not match")
					    fun checkRecord ((name, ty), ()) = case S.look(tenv, ty) of SOME(t) => (tyCheckFormals(name, t); ())
												      | NONE => (ErrorMsg.error pos ("record has invalid type " ^ S.name(typ));()) 

					    val fieldExps = map #exp (map trexp(map #2 fList))
					in
					    if List.length(rFormals) = List.length(fList)
					    then (foldr checkRecord () rFormals;
						  {exp = Tr.transRECORD(fieldExps), ty = t})
					    else (ErrorMsg.error pos ("record " ^ S.name(typ) ^ " has incorrect field list length");
						  {exp = (Tr.transCONST 0), ty = t})
					end)
				     | _ => (ErrorMsg.error pos ("not a record type; got " ^ S.name(typ) ^ " instead");
					     {exp = (Tr.transCONST 0), ty = T.NIL}))
	       | NONE => (ErrorMsg.error pos ("record type not valid: " ^ S.name(typ));
			  {exp = (Tr.transCONST 0), ty = T.NIL}))


	  | trexp (A.ArrayExp{typ = typ, size = size, init = init, pos = pos}) =
	    let val {exp = szExp, ty = szTy} = trexp(size)
		val {exp = inExp, ty = inTy} = trexp(init)
		fun getTy (SOME(ty)) = ty
		  | getTy (NONE) = T.BOTTOM
		fun getActualType ty = case ty of T.NAME(name, tref) => getActualType(getTy(S.look(tenv, name)))
						| t => t 
	    in
		(case S.look(tenv, typ) of SOME(t) =>
					   (case getActualType t of T.ARRAY(ty, uniq) =>
								    (checkInt({exp = szExp, ty = szTy}, pos);
								     equalTypes(inTy, getActualType ty, pos, "mismatching array and exp types");
								     {exp = Tr.transARRAY(szExp, inExp), ty = T.ARRAY(ty, uniq)})
								  | _ => (ErrorMsg.error pos "array not of ARRAY type";
									  {exp = (Tr.transCONST 0), ty = T.BOTTOM}))
					 | NONE => (ErrorMsg.error pos "type does not exist";
						    {exp = (Tr.transCONST 0), ty = T.BOTTOM}))
	    end
		
	and trvar (A.FieldVar(var, sym, pos)) =
	    let val {exp = vExp, ty = vTy} = trvar(var)
	    in case vTy
		of T.RECORD(func, unique) =>
		   let val fields = func ()
		       val (curIndex, ansIndex) =
			   let fun findField (_, (curDex, ansDex)) : (int * int) =
				   let val (sy, ty) = List.nth(fields, curDex)
				   in
				       if S.name(sy) = S.name(sym)
				       then (curDex + 1, curDex)
				       else (curDex + 1, ansDex)
				   end
			   in
			       foldl findField (0, ~1) fields (* init ?*)
			   end
		       fun getFieldTy ((s, t) :: ls, id, pos) =
			   if String.compare(S.name(s), S.name(id)) = EQUAL
			   then case S.look(tenv, t) of SOME(t) => t
						      | NONE => (ErrorMsg.error pos "record field has incorrect type";
								 T.BOTTOM)
			   else getFieldTy(ls, id, pos)
			 | getFieldTy ([], id, pos) = (ErrorMsg.error pos "field does not exist";
						       T.BOTTOM)
		   in
		       {exp = Tr.fieldVar(vExp, ansIndex), ty = getFieldTy(fields, sym, pos)}
		   end
		 | _ => (ErrorMsg.error pos "variable is not a record";
			 {exp = (Tr.transCONST 0), ty = T.BOTTOM}) 
	    end
		
	  | trvar (A.SubscriptVar(var, exp, pos)) =
	    let val dex = trexp(exp)
		val {exp = varExpr, ty = varType} = trvar(var)
		fun getTy (SOME(ty)) = ty
		  | getTy (NONE) = T.BOTTOM
		fun getActualType ty = case ty of T.NAME(name, tref) => getActualType(getTy(S.look(tenv, name)))
						| t => t 
	    in
		(case varType of
		     (T.ARRAY(t, uniq)) => (checkInt(dex, pos);
					    {exp = Tr.subscriptVar(varExpr, #exp(dex)), ty = getActualType(t)})
		   | _ => (ErrorMsg.error pos ("variable is not of ARRAY type");
			   {exp = (Tr.transCONST 0), ty = T.BOTTOM}))
		    
	    end	
	  | trvar (A.SimpleVar(id, pos)) =
	    (case S.look(venv, id)
	      of SOME (E.VarEntry{access, ty, read}) => {exp = Tr.simpleVar(access, level), ty = ty}
	       | SOME(E.FunEntry{...}) => (ErrorMsg.error pos ("improper function call or undefined variable" ^ S.name id); {exp = (Tr.transCONST 0), ty=T.BOTTOM})
	       | NONE => (ErrorMsg.error pos ("undefined variable " ^ S.name id);
			  {exp = (Tr.transCONST 0), ty=T.BOTTOM}))
    in
	trexp exp
    end

and transDecs (venv, tenv, decs, level, doneLabel)  = 
    let fun trdec (venv, tenv, A.VarDec{name, escape, typ = SOME(assignType, typPos), init, pos}, expList) =
	    let val {exp = vExp, ty = typeFound} = transExp(venv, tenv, init, level, doneLabel)
		val varAccess = Tr.allocLocal(level)(!escape)
		fun getTy (SOME(ty)) = ty
		  | getTy (NONE) = T.BOTTOM
		fun getActualType ty = case ty of T.NAME(name, tref) => getActualType(getTy(S.look(tenv, name)))
						| t => t
		val varAssign = Tr.transASSIGN(Tr.simpleVar(varAccess, level), vExp)
	    in
		(case S.look(tenv, assignType) of SOME(t) => (canAssign(getActualType(t), typeFound, pos, "var dec has mismatching types");
							      {venv = S.enter(venv, name, E.VarEntry{access = varAccess, ty = getActualType(t), read = false}),
							       tenv = tenv,
							       expList = varAssign :: expList})
						| NONE => (ErrorMsg.error pos "type is not in environment";
							   {venv = venv, tenv = tenv, expList = (expList @ [varAssign])}))
	    end
	  | trdec (venv, tenv, A.VarDec{name, escape, typ = NONE, init, pos}, expList) =
	    let val {exp = vExp, ty = typeFound} = transExp(venv, tenv, init, level, doneLabel)
		val varAccess = Tr.allocLocal(level)(!escape)
		fun getTy (SOME(ty)) = ty
		  | getTy (NONE) = T.BOTTOM
		fun getActualType ty = case ty of T.NAME(name, tref) => getActualType(getTy(S.look(tenv, name)))
						| t => t
		val varAssign = Tr.transASSIGN(Tr.simpleVar(varAccess, level), vExp)
	    in
		if T.getEQ(typeFound, T.NIL)
		then ErrorMsg.error pos "non record types cannot have type nil"
		else ();
		{venv = S.enter(venv, name, E.VarEntry{access = varAccess, ty = typeFound, read = false}),
		 tenv = tenv, expList = (expList @ [varAssign])}
	    end
		
	  | trdec (venv, tenv, A.FunctionDec(decList), expList) =
	    let fun getFieldType {name, escape, typ, pos} = (case S.look(tenv, typ) of SOME(x) => {name = name, escape = escape, ty = x, pos = pos}
										     | NONE => (ErrorMsg.error 0 ("unrecognized parameter type in function " ^ S.name(typ));
												{name = name, escape = escape, ty = T.BOTTOM, pos = pos}))
		fun getReturnType x = (case S.look(tenv, x) of SOME(t) => t
							     | NONE => (ErrorMsg.error 0 ("unrecognized return type " ^ S.name(x));
									T.BOTTOM))
		fun updateVenv (f, venv) =
		    let fun escapeList {name, escape = e, typ, pos} = !e
			val fLabel = Temp.newlabel()
		    in
			case f of {name, params, body, pos, result = SOME(r, p)} =>
				  S.enter(venv, name, E.FunEntry{level = Tr.newLevel{parent = level, name = fLabel, formals = map escapeList params},
								 label = fLabel,
								 formals = map #ty (map getFieldType params),
								 result = getReturnType(r)})
				| {name, params, body, pos, result = NONE} =>
				  S.enter(venv, name, E.FunEntry{level = Tr.newLevel{parent = level, name = fLabel, formals = map escapeList params},
								 label = fLabel,
								 formals = map #ty (map getFieldType params),
								 result = T.UNIT}) 
		    end
		val venv' = foldr updateVenv venv decList
		fun decTypeCheck ({name, params, body, pos, result}, ()) =
		    let val rt = (case result of SOME(r, p) => getReturnType(r)
					       | NONE => T.UNIT)
			val fLevel = (case S.look(venv', name) of SOME(E.FunEntry({level = l, label, formals, result})) => l
								| _ => Tr.newLevel {parent = Tr.outermost, name = Temp.newlabel(), formals = []})
			val args = map getFieldType params
			val fList = Tr.formals(fLevel)
			fun addParamToVenv ({name, escape, ty, pos}, (venv, index)) =
			    (S.enter(venv, name, E.VarEntry{access = List.nth(fList, index),
							    ty = ty,
							    read = false}), index + 1)
			val updatedVenv = #1 (foldl addParamToVenv (venv', 1) args)
			val body' = transExp(updatedVenv, tenv, body, fLevel, doneLabel)
		    in
			Tr.procEntryExit({level = fLevel, body = #exp(body')});
			if T.getEQ(#ty(body'), rt)
			then ()
			else ErrorMsg.error pos ("mismatching function body type and return type in " ^ S.name(name))
		    end
		fun dupsCheck ({name, params, body, pos, result}, ls) =
		    if List.exists(fn x => String.compare(S.name(name), x) = EQUAL) ls
		    then (ErrorMsg.error pos "mutually recursive functions have same name";
			  ls)
		    else (S.name(name) :: ls)
			     
	    in
		foldl dupsCheck [] decList;
		foldr decTypeCheck () decList;
		{venv = venv', tenv = tenv, expList = expList}
	    end
	  | trdec (venv, tenv, A.TypeDec(decList), expList) =
	    let fun trTypes (tenv, A.ArrayTy(s, p)) = T.ARRAY(trTypes(tenv, A.NameTy(s, p)), ref())
		  | trTypes (tenv, A.NameTy(name, _)) =
		    (case S.look(tenv, name) of SOME _ => T.NAME(name, ref(NONE))
					      | NONE => (ErrorMsg.error 0 ("name type is not recognized: " ^ S.name(name));
							 T.NAME(name, ref(NONE))))
		  | trTypes (tenv, A.RecordTy(fList)) =
		    let fun checkFields {name, escape, typ, pos} = case S.look(tenv, typ) of SOME(_) => (name, typ)
											   | NONE =>(ErrorMsg.error pos ("type of record field " ^ S.name(typ) ^ " is undefined");
												     (name, typ))
			fun composeList (f, ls) = checkFields(f)::ls
			fun createRec () = foldl composeList [] fList
		    in
			createRec();
			T.RECORD(createRec, ref())
		    end
		fun initTypeDec ({name, ty, pos}, tEnv) = S.enter(tEnv, name, T.BOTTOM)
		val tenv' = foldl initTypeDec tenv decList
		fun addToTenv ({name, ty, pos}, {venv, tenv, expList}) = {venv = venv, tenv = S.enter(tenv, name, trTypes(tenv', ty)), expList = expList}
		val tenv'' = foldl addToTenv {venv = venv, tenv = tenv, expList = expList} decList
		fun dupsExist ({name, ty, pos}, ls) = if List.exists (fn x => String.compare(S.name(name), x) = EQUAL) ls
						      then (ErrorMsg.error pos "mutually recursive types has types of same name";
							    ls)
						      else (S.name(name)::ls)
							       
		fun cycleCheck ({name, ty, pos}, ()) =
		    let fun help (ls, name) =
			    (case S.look(#tenv(tenv''), name) of
				 SOME(T.NAME(s, _)) => if List.exists (fn x => String.compare(S.name(s), S.name(x)) = EQUAL) ls
						       then ErrorMsg.error pos "types are mutually recursive types have a cycle and are not part of record or array"
						       else help(name::ls, s)
			       | _ => ())
		    in
			help([], name)
		    end
	    in
		foldl dupsExist [] decList;
		foldl cycleCheck () decList;
		tenv''
	    end
		

	and helper (d, {venv, tenv, expList}) = trdec(venv, tenv, d, expList)
    in
	foldl helper {venv = venv, tenv = tenv, expList = []} decs 
    end

fun transProg(exp : A.exp) =
    let val startLabel = Temp.namedlabel("tig_main")
	val startLevel = Tr.newLevel({parent = Tr.outermost, name = startLabel, formals = []})
	val result = #exp(transExp(E.base_venv, E.base_tenv, exp, startLevel, startLabel))
	val _ = Tr.procEntryExit({level = startLevel, body = result})
	val frags = Tr.getResult()
    in
	frags
    end

end

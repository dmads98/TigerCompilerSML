structure Types =
struct

type unique = unit ref

structure S = Symbol

datatype ty = RECORD of (unit -> (S.symbol * S.symbol) list) * unique
	    | NIL
	    | INT
	    | STRING
	    | ARRAY of ty * unique
	    | NAME of S.symbol * ty option ref
	    | UNIT
	    | BOTTOM

datatype compRes = EQ
		 | LT
		 | GT
		 | NC

fun typeComp (BOTTOM, _) = true
  | typeComp (_, UNIT) = true
  | typeComp (NIL, NIL) = true
  | typeComp (STRING, STRING) = true 
  | typeComp (NIL, RECORD(_)) = true
  | typeComp (INT, INT) = true
  | typeComp (NAME(s1, _), NAME(s2, _)) = String.compare(S.name s1, S.name s2) = EQUAL
  | typeComp (RECORD(_, u1), RECORD(_, u2)) = u1 = u2
  | typeComp (ARRAY(_, u1), ARRAY(_, u2)) = u1 = u2
  | typeComp (_, _) = false

fun getComp (ty1, ty2) = if (typeComp(ty1, ty2) andalso typeComp(ty2, ty1))
			 then EQ
			 else if(typeComp(ty1, ty2))
			 then LT
			 else if (typeComp(ty2, ty1))
			 then GT
			 else NC

fun getEQ (ty1, ty2) = getComp(ty1, ty2) = EQ

fun printType t = case t of RECORD(_, _) => print "record type\n"
			  | NIL => print "nil type\n"
			  | INT => print "int type\n"
			  | STRING => print "string type\n"
			  | ARRAY(arrTy, _) =>  (print "array type of:\n"; printType(arrTy))
			  | NAME(sy, _) => print ("name type of " ^ S.name(sy) ^"\n")
			  | UNIT => print "unit type\n"
			  | BOTTOM => print "bottom type\n"		 
end

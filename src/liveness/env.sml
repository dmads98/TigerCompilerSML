structure Env : ENV =
struct

structure Trans = Translate

type access = unit
type ty = Types.ty
datatype enventry = VarEntry of {access: Translate.access, ty : ty}
		  | FunEntry of {level: Translate.level, label:Temp.label, formals: ty list, result : ty}

val base_tenv = let val map : (ty Symbol.table) = Symbol.empty
		    val tyList = [(Symbol.symbol("string"), Types.STRING), (Symbol.symbol("int"), Types.INT)]
		    fun addToMap ((sym, ty), curMap) = Symbol.enter(curMap, sym, ty)
		in
		    foldl addToMap map tyList
		end

val base_venv = let val map : (enventry Symbol.table) = Symbol.empty
		    val varList = [(Symbol.symbol("exit"), FunEntry{level = Trans.outermost, label = Temp.namedlabel("tigerExit"), formals = [Types.INT], result = Types.UNIT}),
				   (Symbol.symbol("ord"), FunEntry{level = Trans.outermost, label = Temp.namedlabel("tigerOrd"), formals = [Types.STRING], result = Types.INT}),
				   (Symbol.symbol("chr"), FunEntry{level = Trans.outermost, label = Temp.namedlabel("tigerChr"), formals = [Types.INT], result = Types.STRING}),
				   (Symbol.symbol("not"), FunEntry{level = Trans.outermost, label = Temp.namedlabel("tigerNot"), formals = [Types.INT], result = Types.INT}),
				   (Symbol.symbol("size"), FunEntry{level = Trans.outermost, label = Temp.namedlabel("tigerSize"), formals = [Types.STRING], result = Types.INT}),
				   (Symbol.symbol("print"), FunEntry{level = Trans.outermost, label = Temp.namedlabel("tigerPrint"), formals = [Types.STRING], result = Types.UNIT}),
				   (Symbol.symbol("substring"), FunEntry{level = Trans.outermost, label = Temp.namedlabel("tigerSubstring"), formals = [Types.STRING, Types.INT, Types.INT], result = Types.STRING}),
				   (Symbol.symbol("concat"), FunEntry{level = Trans.outermost, label = Temp.namedlabel("tigerConcat"), formals = [Types.STRING, Types.STRING], result = Types.STRING}),
				   (Symbol.symbol("flush"), FunEntry{level = Trans.outermost, label = Temp.namedlabel("tigerFlush"), formals = [], result = Types.UNIT}),
				   (Symbol.symbol("getchar"), FunEntry{level = Trans.outermost, label = Temp.namedlabel("tigerGetChar"), formals = [], result = Types.STRING})
				  ]
		    fun addToMap ((sym, ty), curMap) = Symbol.enter(curMap, sym, ty)
		in
		    foldl addToMap map varList
		end
end

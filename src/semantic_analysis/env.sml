structure Env : ENV =
struct

type access = unit
type ty = Types.ty

	      
datatype enventry = VarEntry of {ty: ty}
		  | FunEntry of {formals: ty list, result: ty}

val base_tenv = let val map : (ty Symbol.table) = Symbol.empty
		    val tyList = [(Symbol.symbol("string"), Types.STRING), (Symbol.symbol("int"), Types.INT)]
		    fun addToMap ((sym, ty), curMap) = Symbol.enter(curMap, sym, ty)
		in
		    foldl addToMap map tyList
		end

val base_venv = let val map : (enventry Symbol.table) = Symbol.empty
		    val varList = [(Symbol.symbol("exit"), FunEntry{formals = [Types.INT], result = Types.UNIT}),
				   (Symbol.symbol("ord"), FunEntry{formals = [Types.STRING], result = Types.INT}),
				   (Symbol.symbol("chr"), FunEntry{formals = [Types.INT], result = Types.STRING}),
				   (Symbol.symbol("not"), FunEntry{formals = [Types.INT], result = Types.INT}),
				   (Symbol.symbol("size"), FunEntry{formals = [Types.STRING], result = Types.INT}),
				   (Symbol.symbol("print"), FunEntry{formals = [Types.STRING], result = Types.UNIT}),
				   (Symbol.symbol("substring"), FunEntry{formals = [Types.STRING, Types.INT, Types.INT], result = Types.STRING}),
				   (Symbol.symbol("concat"), FunEntry{formals = [Types.STRING, Types.STRING], result = Types.STRING}),
				   (Symbol.symbol("flush"), FunEntry{formals = [], result = Types.UNIT}),
				   (Symbol.symbol("getchar"), FunEntry{formals = [], result = Types.STRING}),
				  ]
		    fun addToMap ((sym, ty), curMap) = Symbol.enter(curMap, sym, ty)
		in
		    foldl addToMap map varList
		end
end

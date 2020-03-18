structure Env : ENV =
struct

structure S = Symbol
structure T = Types

type ty = Types.ty
	      
datatype enventry = VarEntry of {access: Translate.access. ty: ty}
       | FunEntry of level: Translate.level,
                                 label: Temp.label,
                                 formals: ty list, result: ty}

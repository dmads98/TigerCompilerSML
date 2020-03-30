structure Trans = Translate

signature ENV =
sig
    type access
    type ty
    datatype enventry = VarEntry of {access: Trans.access, ty : ty}
		      | FunEntry of {level: Trans.level, label: Trans.level, formals: ty list, result: ty}
    val base_tenv : ty Symbol.table (* predefined types *)
    val base_venv : enventry Symbol.table (* predefined functions *)
end

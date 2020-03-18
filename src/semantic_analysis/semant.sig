signature SEMANT =
sig
    val transProg: Absyn.exp -> unit
    val transVar: venv * tenv * Absyn.var -> expty
    val transExp: venv * tenv * Absyn.exp -> expty
    val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
    val transTy: tenv * Absyn.ty -> Types.ty
				
end

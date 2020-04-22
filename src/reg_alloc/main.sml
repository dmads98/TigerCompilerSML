structure Main = struct
structure Tr = Translate
structure F = MipsFrame
(*structure R = RegAlloc*)
		  
fun emitproc out (F.PROC{body,frame}) =
    let val _ = print ("emit " ^ Symbol.name(F.name frame) ^ "\n")
	val _ = Printtree.printtree(TextIO.stdOut,body);
	val stms = Canon.linearize body
	(*         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	val instrs = List.concat(map (MipsGen.codegen frame) stms')
	val updatedInstrs = F.procEntryExit2(frame, instrs)
	val (instrList, alloc) = Reg_Alloc.alloc(updatedInstrs, frame)
        val format0 = Assem.format((fn i => case (Temp.Table.look(alloc, i)) of SOME(a) => a
									      | NONE => (ErrorMsg.error ~1 "was not able to allocate"; Temp.makestring(i))))
    in  app (fn i => TextIO.output(out,format0 i)) instrList
    end
  | emitproc out (F.STRING(lab,s)) = TextIO.output(out, F.string(lab, s)); (* MIPS format string *)

fun withOpenFile fname f = 
    let val out = TextIO.openOut fname
    in (f out before TextIO.closeOut out) 
       handle e => (TextIO.closeOut out; raise e)
    end 

fun compile filename = 
    let val absyn = Parse.parse filename
        val frags = (FindEscape.findEscape absyn; Semant.transProg absyn)

	val runtime = TextIO.inputAll (TextIO.openIn "runtimele.s")
	val sys = TextIO.inputAll (TextIO.openIn "sysspim.s")
    in 
        withOpenFile (filename ^ ".s") 
		     (fn out => (app (emitproc out) frags; TextIO.output(out, runtime); TextIO.output(out, sys)))
    end

end

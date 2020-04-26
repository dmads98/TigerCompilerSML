structure Main = struct
structure Tr = Translate
structure F = MipsFrame
(*structure R = RegAlloc*)

		 
fun getMaxArgs (frags : F.frag list) =
    let fun findMax (F.STRING(_, _), max) = max
	  | findMax (F.PROC ({body, frame}), max) = Int.max(max, List.length(F.formals(frame)))
    in
	foldl findMax 0 frags
    end

fun alterEscapes index = let val boolRefs = FindEscape.getRefs()
			 in
			     if index >= List.length(!boolRefs)
			     then false
			     else if !(List.nth(!boolRefs, index)) = true
			     then alterEscapes(index + 1)
			     else (List.nth(!boolRefs, index) := true;
				   true)
			 end
			     
fun emitproc out (F.PROC{body, frame}) =
    let fun inAlloc (tempList, alloc, reg) =
	    if reg = F.RA
	    then true
	    else let val allRegs = F.getRegTemps(F.specialregs @ F.argregs @ F.calleesaves @ F.callersaves)
		     fun getVal NONE = "INVALID REGISTER"
		       | getVal (SOME(r)) = r
		     fun helper(temp, state) = if isSome(List.find (fn t => temp = t) allRegs)
					       then state
					       else state orelse
						    String.compare(
							getVal(Temp.Table.look(alloc, reg)),
							getVal(Temp.Table.look(alloc, temp))) = EQUAL
		 in
		     foldl helper false tempList
		 end

	val _ = print ("emit " ^ Symbol.name(F.name frame) ^ "\n")
	(* val _ = print("------------Before Linearize--------------\n") *)
	(* val _ = Printtree.printtree(TextIO.stdOut, body); *)
	val stms = Canon.linearize body
	(* val _ = print("------------After Linearize--------------\n") *)
	(* val _ = app (fn s => Printtree.printtree(TextIO.stdOut, s)) stms; *)
        val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
	val instrs = List.concat(map (MipsGen.codegen frame) stms')
	val updatedInstrs = F.procEntryExit2(frame, instrs)
(*	val _ = print("------------INF Register MIPS--------------\n")
	val format1 = Assem.format(fn t => Temp.makestring t)
	val _ = app(fn i => TextIO.output(TextIO.stdOut, format1 i)) updatedInstrs
	val _ = print("------------INF MIPS DONE--------------\n")*)
	val cfg = MakeGraph.instrs2graph(updatedInstrs)
	val interference as Liveness.IGRAPH{graph, tnode, gtemp, moves} = Liveness.interferenceGraph(cfg)
	val (allocation, regsSpilled) = Reg_Alloc.alloc(interference)
	val tempList = map (LG.getNodeID) (LG.nodes (graph))
	val saveTemps = List.filter (fn reg => inAlloc(tempList, allocation, reg)) (F.RA::(F.getRegTemps F.calleesaves))
	val finalInstrs = #body (F.procEntryExit3(frame, instrs, saveTemps, getMaxArgs(Tr.getResult())))
	val format0 = Assem.format(fn i => case (Temp.Table.look(allocation, i)) of SOME(a) => a
										  | NONE => ("REG NOT FOUND"))
    in
	(*print("------------CFG--------------\n");
	MakeGraph.show cfg;
	print("------------Interference Graph--------------\n");
	Liveness.show interference;
	
	print("==================Register allocation " ^ S.name(F.name frame) ^ "========\n");
	Reg_Alloc.printAlloc(allocation, tempList);*)
	print("==================Final Code " ^ S.name(F.name frame) ^ "========\n");
	app (fn i => TextIO.output(TextIO.stdOut, format0 i)) finalInstrs;
	app (fn i => TextIO.output(out, format0 i)) finalInstrs;
	if regsSpilled
	then if alterEscapes(0)
	     then ()
	     else (ErrorMsg.impossible "Unable to perform register allocation")
	else ();
	regsSpilled
(*
	(* val _ = print "orig instrs\n" *)
	(* val _ = app (fn i => TextIO.output(TextIO.stdOut, Assem.format(Temp.makestring) i)) instrs *)
	val _ = print "-----updated instrs------\n"
	val _ = app (fn i => TextIO.output(TextIO.stdOut, Assem.format(F.getRegName) i)) updatedInstrs

	val flowgraph = MakeGraph.instrs2graph(updatedInstrs)
	(* val _ = print("------------CFG--------------\n") *)
	(* val _ = MakeGraph.show flowgraph *)

	val igraph = Liveness.interferenceGraph flowgraph
	(* val _ = print("------------Interference Graph--------------\n") *)
	(* val _ = Liveness.show igraph *)
					      
(*	val (instrList, alloc) = Reg_Alloc.alloc(updatedInstrs, frame)*)
 (*       val format0 = Assem.format((fn i => case (Temp.Table.look(alloc, i)) of SOME(a) => a
									      | NONE => (ErrorMsg.error ~1 "was not able to allocate"; Temp.makestring(i))))*)
    in
	app (fn i => TextIO.output(out, Assem.format(F.getRegName) i)) updatedInstrs *)
    end
  | emitproc out (F.STRING(lab,s)) = (TextIO.output(out, F.string(lab, s)); false) (* MIPS format string *)

fun runtimeOut out =
    let val stream = TextIO.openIn "runtimele.s"
	val _ = TextIO.output(out, TextIO.inputAll stream)
	val _ = TextIO.closeIn stream
    in
	()
    end

fun sysspimOut out =
    let val stream = TextIO.openIn "sysspim.s"
	val _ = TextIO.output(out, TextIO.inputAll stream)
	val _ = TextIO.closeIn stream
    in
	()
    end
	
fun handleTree (tree, filename) =
    let val _ = Translate.reset()
	val _ = print("HANDLETREE\n")
	val frags = Semant.transProg(tree)
		    handle e => OS.Process.exit(OS.Process.success)
	val (strings, procs) = List.partition (fn x =>
						  case x of
						      F.PROC(_) => false
						    | F.STRING(_) => true) frags
	(* val _ = PrintAbsyn.print(TextIO.stdOut, tree) *)
	val out = TextIO.openOut(filename ^ ".s")
	val _ = TextIO.output(out, ".data\n")
	val _ = foldl (fn (proc, spilled) => spilled orelse
					     (emitproc out proc)) false strings
		handle e => (TextIO.closeOut out; raise e)
	val _ = TextIO.output(out, ".text\n")
			  (* TextIO.output(out, ".text\n.globl tig_main\n.ent tig_main\n");*)
	val _ = runtimeOut out

	val regsSpilled = foldl (fn (proc, spilled) => spilled orelse
						       (emitproc out proc)) false procs
		handle e => (TextIO.closeOut out; raise e)
			     (*TextIO.output(out, "#-----------tig_main----------\n");*)
	val _ = sysspimOut out
	val _ = TextIO.closeOut out
    in
	if regsSpilled
	then handleTree(tree, filename)
	else ()
    end

fun compile filename =
    let
	val t = ErrorMsg.reset()
	val _ = Translate.reset()
	val _ = FindEscape.reset()
	val _ = Temp.reset()
	val tree = Parse.parse filename
		   handle e => OS.Process.exit(OS.Process.success)
	val _ = FindEscape.findEscape(tree)
    in
	handleTree(tree, filename)
    end
end	  

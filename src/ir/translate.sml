structure T = Tree

structure Translate =
struct datatype exp = Ex of Tree.exp
		    | Nx of Tree.stm
		    | Cx of Temp.label * Temp.label -> Temp.stm
       
       (* frag list ref local *)
       
       val procEntryExit : {level: level, body: exp} -> unit (* Remember proc fragments *)

       structure Frame : FRAME
       val getResult : unit -> Frame.frag list
       val newLevel (* Function *)
       
       fun unEx (Ex e) = e
	 | unEx (Cx genstm) =
	   let val r = Temp.newtemp()
	       val t  = Temp.newlabel() and f = Temp.newlabel ()
	   in T.ESEQ(seq[T.MOVE(T.TEMP r, T.const 1),
			 genstm(t, f),
			 T.LABEL f,
			 T.MOVE(T.Temp r, T.CONST 0),
			 T.LABEL t],
		     T.TEMP r)
	   end
	 | unEx (Nx s) = T.ESEQ(s, T.CONST 0)

       fun unNx (Ex e) = e
	 | unNx (Cx s) = s
	 | unNx (Nx f) =	   
	   let val x = Temp.newlabel ()
	   in SEQ(f(x, x), T.LABEL x)
	   end

       fun unCx (Ex e) = e
	 | unCx (Cx s) = s
	 | unCx (Nx f) =
	   
end;
(* Assume every variable escapes, keep in local frame and don't bother with findEscape *)

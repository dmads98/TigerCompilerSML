Prathikshaa Rangarajan (pr109)
Dhanush Madabusi (dm322)

* Compile and Run: *

CM.make "sources.cm";
Main.compile "/path/to/tiger/file";

 - In case of syntax or type check errors, the sml process is aborted after informing you of the error.
 - In case of successful compilation, a *.tig.s file is generated in the same directory as the *.tig file.

* Fixes from previous stages: *
  ** Parser **
  - Parses valid syntax correctly
  - 
 - Fixed string MIPS syntax.
 - Fixed program exit.
 - Does not jump to non-existant labels
 - 

* Some Improvements and Optimizations: *
 - In place math operations => reduces the binops generated.
 - Removing unneccessary moves.
 - Coalescing in register allocation.
 - 

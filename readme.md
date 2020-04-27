# Tiger Compiler in SML

**Developers**: Prathikshaa Rangarajan (pr109), Dhanush Madabusi (dm322)  
**Language**:   SML

This compiler is for Andrew Appel's Tiger language and was written in SML. This project is part of Professor Drew Hilton's ECE 553 Compiler Construction class at Duke.  

## Setup and Run
- The final compiler is in the "reg_alloc/" directory. 
- The following can be used to build the compiler then compile a tiger file.  
```
CM.make "sources.cm";
Main.main "/path/to/tiger/file";
```
This generates the MIPS assembly file  `filename.tig.s` in the same directory as `filename.tig`. 

## Test Scripts
- To compile the correct tests in the "/src/testcases" directory, you may run `sml < testscript` in "/src/reg_alloc".

Additionally, "/src/reg_alloc" contains a bash script `compile.sh` which can generate the sml code required to compile tests in any directory. Use as:
```
chmod +x *.sh
./compile.sh /path/to/tig
```
e.g. `./compile.sh /src/testcases/correct` which generates a file `compile.sml` in the current directory. This can be used in a manner similar to `testscript`.

Copy "compile.sml" to the directory containing the source code - reg_alloc and run as `sml < compile.sml`.

Then to run the generated assembly in "/src/testcases/correct/", just run the script `./spim.sh` which runs all available .s files in the same directory.

## Test Cases
- The provided tests in "/src/testcases" aer divided into two sets "/src/testcases/correct" and "/src/testcases/failure" based on whether the tiger code is supposed to compile or fail during the compilation process either due to syntax or type check errors.
- The test cases in "/src/testcases/correct" have been updated to print out all the data that is being used in the tests to verify correctness.
- Any tests such as the ones in "/src/testcases/failure" when compiled in SML using Main.compile "/path/to/tig/file" throw the appropriate error and exit the SML process and do not generate a .s file.

## Optimizations

* We create a type lattice structure in types.sml with a BOTTOM type.
  * As well, there are functions to determine if type a is equal to, a subtype of, or a supertype of type b.
* We have created **purely functional types** in our typechecker.
  * For records, we pass a (unit -> (S.symbol * S.symbol) list) function that generates record fields, instead of using Types.NAME and refs to check for recursive declarations.
* In our IR phase, we have added numerous optimizations to reduce the number instructions created, binops generated, and CJUMPS. Anywhere we see Tree.CONSTs, we attempt to in place SML operations rather than create more instructions. Examples are given below:
  * CONSTs are used instead of BINOPs if operation can be done in place
  * Multiplication is done by LSHIFT where possible
  * Addition by 0, Subtraction by 0, or Multiplication by 0 and 1 do not create instructions
  * If test conditions can be predetermined, then if-then-else statements, for loops, and while loops do not create instructions
  * If RELOP results can be predetermined, then JUMPS are created instead of CJUMPS.
  * We use $zero where needed instead of a load immediate instruction
* In our Register Allocation, we have implemented **basic spill handling and coalescing**.
 
## Minor Improvements
* In case of syntax or type check errors, the sml process is aborted after informing you of the error.
* We added a "read" field to Env.VarEntry with the purpose of making for-loop variables read only.
* We added a type pos to tree.sml, which is the first arg of the MOVE type.
* String literals in .data of files are unique.
* ProcEntryExit only saves and restores callee-saved registers that were used.


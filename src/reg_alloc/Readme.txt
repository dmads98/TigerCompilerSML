Prathikshaa Rangarajan (pr109)
Dhanush Madabusi (dm322)

This directory contains the final compiler, including working liveness analysis and register allocation.
To test all files, you can run "sml < testscript" at the shell, which will generate ".tig.s" files in the same directory as the test file.

Or to compile and run manually, use:

CM.make "sources.cm";
Main.main "/path/to/tiger/file";

We lost points on for errors in previous phases and have made the following fixes to our compiler to deal with all of them.
Parser:
- Call Exp correctly parses parameters
- No longer errors on valid programs in terms of empty in...end or side effects

IR & Instruction Selection:
- String are now formatted correctly
  - Appel only gave his (partially incorrect) instructions on this in Chapter 12, past where we were at in the class. Hilton only gave us the correct format after Instruction Selection was due. So we believe we should not have lost points on this.
- Array size is now correctly stored at -4 not 0.
  - We believe Austin may have inadvertently deducted -4 for this instead of a smaller amount more reasonable for the bug.
- No longer non existent labels for jump targets
  - Austin seems to have mistakently deducted us twice for this instead of once.
- Programs correctly exit, rather than jumping to labels with nothing
  - Hilton had not provided runtimele.s and sysspim.s that allow programs to properly exit, so we believe we should not have lost points for this. 
- Records correctly have nil checks
- Results of functions are correctly read from $v0
- Main is not omitted in some instances, which caused some program entry to be in middle of process call.
  - Was not able to reproduce this
- Malloc correctly getting number of bytes to be allocated not number of words

#!/bin/bash

COMPILE_DIR=$1
TIG_FILES=${COMPILE_DIR}*.tig

# echo $TIG_FILES

FILE="compile.sml"
echo "(* Generating Compilation Script *)" > $FILE
echo "CM.make \"sources.cm\";" >> $FILE;

for a in $TIG_FILES;
do echo "(*-------" $a "-------*)" >> $FILE;
   echo "Main.compile \""$a"\";" >> $FILE;
done;

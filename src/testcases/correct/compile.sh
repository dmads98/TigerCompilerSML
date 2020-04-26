
FILE="compile.sml"
echo "(* Generating Compilation Script *)" > $FILE
for a in ~/compiler/src/testcases/correct/*.tig;
do echo "(*-------" $a "-------*)" >> $FILE;
   echo "CM.make \"sources.cm\";" >> $FILE;
   echo "Main.compile " $a ";" >> $FILE;
done;

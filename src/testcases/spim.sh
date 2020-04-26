for a in *.s;

do echo "-----------------" $a "------------------";
   spim -file $a;
   
done

Dhanush Madabusi (dm322)
Prathikshaa Rangarajan (pr109)

CHAP-4
-------------------------------
This is the version of the parser built for chap-4 after adding symantic actions.
It was verified that all the provided test tiger programs can parse without throwing any errors before moving onto semantic actions.

Shift Reduce Conflicts:
* Our grammar results in three shift reduce conflicts:
  - lvalue [] vs arr[] of ID
  This conflict has been modified to produce the results we desire by adding the redundant ID [] rule to lvalue,
  thus allowing the default shift LBRACK to include both lvalue and array creation into the next state.

  - typelist conflict on keyword TYPE -- default shift for conflict resolution is acceptable here.
  - funclist conflict on keyword FUNC -- default shift for conflict resolution is acceptable here.


TO RUN:
CM.make "sources.cm";
Parse.parse "*.tig";

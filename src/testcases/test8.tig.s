.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L2:
sw $a0, 0($fp)
li $v0, 40
j L1 
L1:

.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L2:
sw $a0, 0($fp)
li t140, 4
li $v0, 0
j L1 
L1:

.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L202:
sw $a0, 0($fp)
li t307, 0
addi t308, t307, 3
move $v0, t308
j L201 
L201:

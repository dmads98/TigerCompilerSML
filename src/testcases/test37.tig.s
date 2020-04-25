.data
.align 4
L1: .asciiz " "
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L3:
sw $a0, 0($fp)
li t140, 0
la t141, L1
li $v0, 0
j L2 
L2:

.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L6:
sw $a0, 0($fp)
li t140, 0
li t141, 1
beq t140, $zero, L1 
L2:
li t141, 0
L1:
li t142, 1
bne t140, $zero, L3 
L4:
li t142, 0
L3:
move $v0, t142
j L5 
L5:

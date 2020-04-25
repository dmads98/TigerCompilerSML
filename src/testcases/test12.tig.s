.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L5:
sw $a0, 0($fp)
li t140, 0
sw $zero, -48($fp)
lw t141, -48($fp)
li t142, 100
ble t141, t142, L2 
L1:
li $v0, 0
j L4 
L2:
addi t143, t140, 1
move t140, t143
lw t144, -48($fp)
li t145, 100
bge t144, t145, L1 
L3:
lw t147, -48($fp)
addi t146, t147, 1
sw t146, -48($fp)
j L2 
L4:

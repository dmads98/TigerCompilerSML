.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L6:
sw $a0, 0($fp)
move $a0, $fp
li $a1, 10
jal L1
j L5 
L5:
L8:
sw $a0, 0($fp)
move t140, $a1
beq t140, $zero, L2 
L3:
move t143, t140
lw t144, 0($fp)
move $a0, t144
addi t145, t140, -1
move $a1, t145
jal L1
move t142, $v0
mul t146, t143, t142
move t141, t146
L4:
move $v0, t141
j L7 
L2:
li t141, 1
j L4 
L7:

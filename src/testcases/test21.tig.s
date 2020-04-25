.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L113:
sw $a0, 0($fp)
move $a0, $fp
li $a1, 10
jal L108
j L112 
L112:
L115:
sw $a0, 0($fp)
move t199, $a1
beq t199, $zero, L109 
L110:
move t202, t199
lw t203, 0($fp)
move $a0, t203
addi t204, t199, -1
move $a1, t204
jal L108
move t201, $v0
mul t205, t202, t201
move t200, t205
L111:
move $v0, t200
j L114 
L109:
li t200, 1
j L111 
L114:

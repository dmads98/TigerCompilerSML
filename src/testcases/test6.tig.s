L41:str2
L40:str
L43:
addi t196, $sp, 0
move $sp, t196
lw t197, 0($fp)
move $a0, t197
addi t198, t193, 1
move $a1, t198
jal L39
addi t199, $sp, 0
move $sp, t199
move $v0, $v0
j L42 
L42:
L45:
addi t200, $sp, 0
move $sp, t200
lw t201, 0($fp)
move $a0, t201
move $a1, t195
la t202, L40
move $a2, t202
jal L38
addi t203, $sp, 0
move $sp, t203
move $v0, $v0
j L44 
L44:
L47:
addi t204, $sp, 0
move $sp, t204
move $a0, $fp
li t205, 0
move $a1, t205
la t206, L41
move $a2, t206
jal L38
addi t207, $sp, 0
move $sp, t207
move $v0, $v0
j L46 
L46:

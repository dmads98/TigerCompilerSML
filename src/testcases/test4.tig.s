L33:
li t175, 0
beq t171, t175, L29 
L30:
move t174, t171
addi t176, $sp, 0
move $sp, t176
lw t177, 0($fp)
move $a0, t177
li t179, 1
sub t178, t171, t179
move $a1, t178
jal L28
addi t180, $sp, 0
move $sp, t180
move t173, $v0
mul t181, t174, t173
move t172, t181
L31:
move $v0, t172
j L32 
L29:
li t182, 1
move t172, t182
j L31 
L32:
L35:
addi t183, $sp, 0
move $sp, t183
move $a0, $fp
li t184, 10
move $a1, t184
jal L28
addi t185, $sp, 0
move $sp, t185
move $v0, $v0
j L34 
L34:

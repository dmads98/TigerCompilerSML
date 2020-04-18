L13:str2
L12:str
L15:
addi t135, $sp, 0
move $sp, t135
lw t136, 0($fp)
move $a0, t136
addi t137, t132, 1
move $a1, t137
jal L11
addi t138, $sp, 0
move $sp, t138
move $v0, $v0
j L14 
L14:
L17:
addi t139, $sp, 0
move $sp, t139
lw t140, 0($fp)
move $a0, t140
move $a1, t134
la t141, L12
move $a2, t141
jal L10
addi t142, $sp, 0
move $sp, t142
move $v0, $v0
j L16 
L16:
L19:
addi t143, $sp, 0
move $sp, t143
move $a0, $fp
li t144, 0
move $a1, t144
la t145, L13
move $a2, t145
jal L10
addi t146, $sp, 0
move $sp, t146
move $v0, $v0
j L18 
L18:

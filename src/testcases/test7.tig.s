L52:str2
L51: 
L50:str
L54:
addi t211, $sp, 0
move $sp, t211
lw t212, 0($fp)
move $a0, t212
addi t213, t208, 1
move $a1, t213
jal L49
addi t214, $sp, 0
move $sp, t214
li t215, 0
move $v0, t215
j L53 
L53:
L56:
addi t216, $sp, 0
move $sp, t216
lw t217, 0($fp)
move $a0, t217
move $a1, t210
la t218, L50
move $a2, t218
jal L48
addi t219, $sp, 0
move $sp, t219
la t220, L51
move $v0, t220
j L55 
L55:
L58:
addi t221, $sp, 0
move $sp, t221
move $a0, $fp
li t222, 0
move $a1, t222
la t223, L52
move $a2, t223
jal L48
addi t224, $sp, 0
move $sp, t224
move $v0, $v0
j L57 
L57:

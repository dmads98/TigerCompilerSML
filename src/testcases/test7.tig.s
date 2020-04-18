L94:str2
L93: 
L92:str
L96:
addi t387, $sp, 0
move $sp, t387
lw t388, 0($fp)
move $a0, t388
addi t389, t384, 1
move $a1, t389
jal L91
addi t390, $sp, 0
move $sp, t390
li t391, 0
move $v0, t391
j L95 
L95:
L98:
addi t392, $sp, 0
move $sp, t392
lw t393, 0($fp)
move $a0, t393
move $a1, t386
la t394, L92
move $a2, t394
jal L90
addi t395, $sp, 0
move $sp, t395
la t396, L93
move $v0, t396
j L97 
L97:
L100:
addi t397, $sp, 0
move $sp, t397
move $a0, $fp
li t398, 0
move $a1, t398
la t399, L94
move $a2, t399
jal L90
addi t400, $sp, 0
move $sp, t400
move $v0, $v0
j L99 
L99:

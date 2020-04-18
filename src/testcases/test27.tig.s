L72:
move $v0, t240
j L71 
L71:
L74:
li t241, 0
move t239, t241
addi t242, $sp, 0
move $sp, t242
move $a0, $fp
li t243, 2
move $a1, t243
jal L70
addi t244, $sp, 0
move $sp, t244
move $v0, $v0
j L73 
L73:

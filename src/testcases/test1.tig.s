L21:
addi t149, $sp, 0
move $sp, t149
li t151, 10
addi t150, t151, 1
move $a0, t150
li t152, 0
move $a1, t152
jal initArray
addi t153, $sp, 0
move $sp, t153
move t147, $v0
li t154, 10
sw t154, 0(t147) 
move t148, t147
move $v0, t148
j L20 
L20:

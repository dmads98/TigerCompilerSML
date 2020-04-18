L23:
addi t157, $sp, 0
move $sp, t157
li t159, 10
addi t158, t159, 1
move $a0, t158
li t160, 0
move $a1, t160
jal initArray
addi t161, $sp, 0
move $sp, t161
move t155, $v0
li t162, 10
sw t162, 0(t155) 
move t156, t155
move $v0, t156
j L22 
L22:

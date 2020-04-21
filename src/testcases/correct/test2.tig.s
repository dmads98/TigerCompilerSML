L11:
addi t134, $sp, 0
move $sp, t134
li t136, 10
addi t135, t136, 1
move $a0, t135
li t137, 0
move $a1, t137
jal initArray
addi t138, $sp, 0
move $sp, t138
move t132, $v0
li t139, 10
sw t139, 0(t132) 
addi t140, t132, 4
move t133, t140
move $v0, t133
j L10 
L10:

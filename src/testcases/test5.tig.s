L37:
addi t188, $sp, 0
move $sp, t188
li t189, 2
move $a0, t189
jal allocRecord
addi t190, $sp, 0
move $sp, t190
move t186, $v0
li t191, 0
sw 's0, 0(t186) 
li t192, 0
sw 's0, 4(t186) 
move t187, t186
move $v0, t187
j L36 
L36:

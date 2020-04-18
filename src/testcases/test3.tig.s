L25:Somebody
L24:Nobody
L27:
addi t165, $sp, 0
move $sp, t165
li t166, 2
move $a0, t166
jal allocRecord
addi t167, $sp, 0
move $sp, t167
move t163, $v0
la t168, L24
sw 's0, 0(t163) 
li t169, 1000
sw 's0, 4(t163) 
move t164, t163
la t170, L25
sw 's0, 0(t164) 
move $v0, t164
j L26 
L26:

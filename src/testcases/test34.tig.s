.data
.align 4
L153: .asciiz "two"
L152: .asciiz "one"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L155:
sw $a0, 0($fp)
move $a0, $fp
la $a1, L152
la $a2, L153
jal L151
j L154 
L154:
L157:
sw $a0, 0($fp)
move t236, $a1
move t237, $a2
move $v0, t236
j L156 
L156:

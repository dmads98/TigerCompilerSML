.data
.align 4
L165: .asciiz "one"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L167:
sw $a0, 0($fp)
move $a0, $fp
li $a1, 3
la $a2, L165
li $a3, 5
jal L164
j L166 
L166:
L169:
sw $a0, 0($fp)
move t240, $a1
move t241, $a2
move $v0, t240
j L168 
L168:

.data
.align 4
L85: .asciiz "str2"
L84: .asciiz " "
L83: .asciiz "str"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L87:
sw $a0, 0($fp)
li t190, 0
move $a0, $fp
li $a1, 0
la $a2, L85
jal L81
j L86 
L86:
L89:
sw $a0, 0($fp)
move t191, $a1
lw t192, 0($fp)
move $a0, t192
move $a1, t191
la $a2, L83
jal L81
la $v0, L84
j L88 
L88:
L91:
sw $a0, 0($fp)
move t188, $a1
move t189, $a2
li $v0, 0
j L90 
L90:

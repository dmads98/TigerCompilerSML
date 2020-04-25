.data
.align 4
L96: .asciiz "str2"
L95: .asciiz " "
L94: .asciiz "str"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L98:
sw $a0, 0($fp)
move $a0, $fp
li $a1, 0
la $a2, L96
jal L93
j L97 
L97:
L100:
sw $a0, 0($fp)
move t194, $a1
move t195, $a2
lw t196, 0($fp)
move $a0, t196
addi t197, t194, 1
move $a1, t197
jal L92
li $v0, 0
j L99 
L99:
L102:
sw $a0, 0($fp)
move t193, $a1
lw t198, 0($fp)
move $a0, t198
li $a1, 0
la $a2, L94
jal L93
la $v0, L95
j L101 
L101:

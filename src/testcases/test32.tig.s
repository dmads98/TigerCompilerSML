.data
.align 4
L146: .asciiz " "
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L148:
sw $a0, 0($fp)
li $a0, 10
la $a1, L146
jal tig_initArray
move t234, $v0
li $v0, 0
j L147 
L147:

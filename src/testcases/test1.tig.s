.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L2:
sw $a0, 0($fp)
li $a0, 10
li $a1, 0
jal tig_initArray
move t132, $v0
move $v0, t132
j L1 
L1:

.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L140:
sw $a0, 0($fp)
li $a0, 10
li $a1, 0
jal tig_initArray
move t224, $v0
move $v0, t224
j L139 
L139:

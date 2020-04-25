.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L75:
sw $a0, 0($fp)
li t185, 1
li t186, 20
beq t185, t186, L72 
L73:
li $v0, 0
j L74 
L72:
j L73 
L74:

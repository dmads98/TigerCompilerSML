.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L2:
sw $a0, 0($fp)
li $a0, 8
jal tig_allocRecord
move t140, $v0
sw $zero, 0(t140)
sw $zero, 4(t140)
move t141, t140
move $v0, t141
j L1 
L1:

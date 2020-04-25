.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L3:
sw $a0, 0($fp)
li t140, 0
move $a0, $fp
li $a1, 2
jal L1
j L2 
L2:
L5:
sw $a0, 0($fp)
move t141, $a1
move $v0, t141
j L4 
L4:

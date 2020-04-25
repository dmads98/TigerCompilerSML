.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L4:
sw $a0, 0($fp)
li $v0, 0
j L3 
L3:
L6:
sw $a0, 0($fp)
move t141, $a1
move $v0, t141
j L5 
L5:
L8:
sw $a0, 0($fp)
move t140, $a1
move $v0, t140
j L7 
L7:

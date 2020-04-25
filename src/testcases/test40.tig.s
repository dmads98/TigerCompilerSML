.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L185:
sw $a0, 0($fp)
move $a0, $fp
li $a1, 2
jal L183
j L184 
L184:
L187:
sw $a0, 0($fp)
move t246, $a1
move $v0, t246
j L186 
L186:

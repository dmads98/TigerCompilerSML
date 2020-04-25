.data
.align 4
L136: .asciiz "Name"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L138:
sw $a0, 0($fp)
li $a0, 8
jal tig_allocRecord
move t221, $v0
sw $zero, 0(t221)
la t223, L136
sw t223, 4(t221)
move t222, t221
move $v0, t222
j L137 
L137:

.data
.align 4
L159: .asciiz "one"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L161:
sw $a0, 0($fp)
move $a0, $fp
la $a1, L159
jal L158
j L160 
L160:
L163:
sw $a0, 0($fp)
move t238, $a1
move t239, $a2
move $v0, t238
j L162 
L162:

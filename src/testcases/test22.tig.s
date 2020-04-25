.data
.align 4
L117: .asciiz "asd"
L116: .asciiz "Name"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L119:
sw $a0, 0($fp)
li $a0, 8
jal tig_allocRecord
move t206, $v0
sw $zero, 0(t206)
la t208, L116
sw t208, 4(t206)
move t207, t206
la t209, L117
sw t209, -4(t207)
li $v0, 0
j L118 
L118:

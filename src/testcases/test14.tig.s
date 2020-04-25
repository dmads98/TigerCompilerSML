.data
.align 4
L66: .asciiz "aname"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L71:
sw $a0, 0($fp)
li $a0, 8
jal tig_allocRecord
move t180, $v0
sw $zero, 0(t180)
la t184, L66
sw t184, 4(t180)
move t181, t180
li $a0, 3
li $a1, 0
jal tig_initArray
move t182, $v0
bne t181, t182, L67 
L68:
li t183, 4
L69:
move $v0, t183
j L70 
L67:
li t183, 3
j L69 
L70:

.data
.align 4
L121: .asciiz ""
L120: .asciiz "aname"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L123:
sw $a0, 0($fp)
li $a0, 8
jal tig_allocRecord
move t210, $v0
sw $zero, 0(t210)
la t212, L120
sw t212, 4(t210)
move t211, t210
li t213, 3
sw t213, 4(t211)
la t214, L121
sw t214, 0(t211)
li $v0, 0
j L122 
L122:

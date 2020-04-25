.data
.align 4
L2: .asciiz "Somebody"
L1: .asciiz "Nobody"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L4:
sw $a0, 0($fp)
li $a0, 8
jal tig_allocRecord
move t140, $v0
li t142, 1000
sw t142, 0(t140)
la t143, L1
sw t143, 4(t140)
move t141, t140
la t144, L2
sw t144, 4(t141)
move $v0, t141
j L3 
L3:

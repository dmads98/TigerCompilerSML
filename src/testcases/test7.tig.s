.data
.align 4
L5: .asciiz "str2"
L4: .asciiz " "
L3: .asciiz "str"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L7:
sw $a0, 0($fp)
move $a0, $fp
li $a1, 0
la $a2, L5
jal L2
j L6 
L6:
L9:
sw $a0, 0($fp)
move t141, $a1
move t142, $a2
lw t143, 0($fp)
move $a0, t143
addi t144, t141, 1
move $a1, t144
jal L1
li $v0, 0
j L8 
L8:
L11:
sw $a0, 0($fp)
move t140, $a1
lw t145, 0($fp)
move $a0, t145
move $a1, t140
la $a2, L3
jal L2
la $v0, L4
j L10 
L10:

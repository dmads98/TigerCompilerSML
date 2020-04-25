.data
.align 4
L4: .asciiz "str2"
L3: .asciiz "str"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L6:
sw $a0, 0($fp)
move $a0, $fp
li $a1, 0
la $a2, L4
jal L2
j L5 
L5:
L8:
sw $a0, 0($fp)
move t141, $a1
move t142, $a2
lw t143, 0($fp)
move $a0, t143
addi t144, t141, 1
move $a1, t144
jal L1
j L7 
L7:
L10:
sw $a0, 0($fp)
move t140, $a1
lw t145, 0($fp)
move $a0, t145
move $a1, t140
la $a2, L3
jal L2
j L9 
L9:

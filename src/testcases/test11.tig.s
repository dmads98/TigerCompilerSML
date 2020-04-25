.data
.align 4
L50: .asciiz " "
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L55:
sw $a0, 0($fp)
li t160, 10
sw t160, -48($fp)
lw t161, -48($fp)
la t162, L50
ble t161, t162, L52 
L51:
li $v0, 0
j L54 
L52:
lw t164, -48($fp)
addi t163, t164, -1
sw t163, -48($fp)
lw t165, -48($fp)
la t166, L50
bge t165, t166, L51 
L53:
lw t168, -48($fp)
addi t167, t168, 1
sw t167, -48($fp)
j L52 
L54:

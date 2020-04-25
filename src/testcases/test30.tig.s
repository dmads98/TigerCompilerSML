.data
.align 4
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L2:
sw $a0, 0($fp)
li $a0, 10
li $a1, 0
jal tig_initArray
move t140, $v0
li t145, 2
addi t144, t145, 1
li t146, 4
mul t143, t144, t146
add t142, t140, t143
move t141, t142
lw t147, 0(t141)
move $v0, t147
j L1 
L1:

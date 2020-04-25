.data
.align 4
L143: .asciiz " "
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L145:
sw $a0, 0($fp)
la t233, L143
move $v0, t233
j L144 
L144:

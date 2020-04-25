.data
.align 4
L128: .asciiz "var"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L130:
sw $a0, 0($fp)
la t218, L128
addi t217, t218, 3
move $v0, t217
j L129 
L129:

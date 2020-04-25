.data
.align 4
L76: .asciiz ""
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L78:
sw $a0, 0($fp)
la $v0, L76
j L77 
L77:

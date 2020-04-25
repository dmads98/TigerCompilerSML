.data
.align 4
L61: .asciiz "df"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L65:
sw $a0, 0($fp)
li t177, 1
li t178, 3
la t179, L61
bgt t178, t179, L62 
L63:
li t177, 0
L62:
move $v0, t177
j L64 
L64:

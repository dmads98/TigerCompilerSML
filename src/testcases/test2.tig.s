L79:
addi t363, $sp, 0
move $sp, t363
li t365, 10
addi t364, t365, 1
move $a0, t364
li t366, 0
move $a1, t366
jal initArray
addi t367, $sp, 0
move $sp, t367
move t361, $v0
li t368, 10
sw t368, 0(t361) 
move t362, t361
move $v0, t362
j L78 
L78:

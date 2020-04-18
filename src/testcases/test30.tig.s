L79:
addi t249, $sp, 0
move $sp, t249
li t251, 10
addi t250, t251, 1
move $a0, t250
li t252, 0
move $a1, t252
jal initArray
addi t253, $sp, 0
move $sp, t253
move t245, $v0
li t254, 10
sw t254, 0(t245) 
move t246, t245
li t255, 2
move t247, t255
move t248, t246
lw t256, 0(t248)
bge t247, t256, L75 
L77:
li t257, 0
blt t247, t257, L75 
L76:
addi t261, t247, 1
li t262, 4
mul t260, t261, t262
add t259, t248, t260
lw t258, 0(t259)
move $v0, t258
j L78 
L75:
addi t263, $sp, 0
move $sp, t263
li t264, 1
move $a0, t264
jal exit
addi t265, $sp, 0
move $sp, t265
j L76 
L78:

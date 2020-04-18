L68:
li t233, 0
move t230, t233
li t234, 0
move t231, t234
li t235, 100
move t232, t235
ble t231, t232, L66 
L64:
li t236, 0
move $v0, t236
j L67 
L65:
addi t237, t231, 1
move t231, t237
L66:
addi t238, t230, 1
move t230, t238
blt t231, t232, L65 
L69:
j L64 
L67:

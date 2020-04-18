L63:
li t226, 10
li t227, 20
bgt t226, t227, L59 
L60:
li t228, 40
move t225, t228
L61:
move $v0, t225
j L62 
L59:
li t229, 30
move t225, t229
j L61 
L62:

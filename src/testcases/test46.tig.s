L123:
li t431, 0
move t429, t431
L119:
li t432, 1
move t430, t432
li t433, 0
bne t429, t433, L120 
L121:
li t434, 0
move t430, t434
L120:
move $v0, t430
j L122 
L122:

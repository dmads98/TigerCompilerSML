.data
.align 4
L33: .asciiz "\n"
L24: .asciiz " ."
L23: .asciiz " O"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L37:
sw $a0, 0($fp)
li $gp, 8
sw $gp, -48($fp)
addi $sp, $fp, -52
move $s3, $sp
lw $a0, -48($fp)
li $a1, 0
jal tig_initArray
move $s2, $v0
sw $s2, 0($s3)
addi $fp, $fp, -56
move $s5, $fp
lw $a0, -48($fp)
li $a1, 0
jal tig_initArray
move $s4, $v0
sw $s4, 0($s5)
addi $ra, $fp, -60
move $s7, $ra
lw t134, -48($fp)
lw t135, -48($fp)
add t133, t134, t135
addi t132, t133, -1
move $a0, t132
li $a1, 0
jal tig_initArray
move $s6, $v0
sw $s6, 0($s7)
addi t136, $fp, -64
move $k1, t136
lw t139, -48($fp)
lw t140, -48($fp)
add t138, t139, t140
addi t137, t138, -1
move $a0, t137
li $a1, 0
jal tig_initArray
move $k0, $v0
sw $k0, 0($k1)
move $a0, $fp
li $a1, 0
jal L1
j L36 
L36:
L39:
sw $a0, 0($fp)
sw $zero, -48($fp)
lw t141, -48($fp)
lw t144, 0($fp)
lw t143, -48(t144)
addi t142, t143, -1
ble t141, t142, L34 
L21:
la $a0, L33
jal tig_print
j L38 
L34:
sw $zero, -52($fp)
lw t145, -52($fp)
lw t148, 0($fp)
lw t147, -48(t148)
addi t146, t147, -1
ble t145, t146, L31 
L22:
la $a0, L33
jal tig_print
lw t149, -48($fp)
lw t152, 0($fp)
lw t151, -48(t152)
addi t150, t151, -1
bge t149, t150, L21 
L35:
lw t154, -48($fp)
addi t153, t154, 1
sw t153, -48($fp)
j L34 
L31:
lw t157, 0($fp)
lw t156, -56(t157)
lw t160, -48($fp)
addi t159, t160, 1
li t161, 4
mul t158, t159, t161
add t155, t156, t158
move $t8, t155
lw t162, 0($t8)
lw t163, -52($fp)
beq t162, t163, L25 
L26:
la $t9, L24
L27:
move $a0, $t9
jal tig_print
lw t164, -52($fp)
lw t167, 0($fp)
lw t166, -48(t167)
addi t165, t166, -1
bge t164, t165, L22 
L32:
lw t169, -52($fp)
addi t168, t169, 1
sw t168, -52($fp)
j L31 
L25:
la $t9, L23
j L27 
L38:
L41:
sw $a0, 0($fp)
move $zero, $a1
lw t171, 0($fp)
lw t170, -48(t171)
beq $zero, t170, L18 
L19:
sw $zero, -48($fp)
lw t172, -48($fp)
lw t175, 0($fp)
lw t174, -48(t175)
addi t173, t174, -1
ble t172, t173, L16 
L3:
li $t7, 0
L20:
move $v0, $t7
j L40 
L18:
lw t176, 0($fp)
move $a0, t176
jal L2
move $t7, $v0
j L20 
L16:
lw t179, 0($fp)
lw t178, -52(t179)
lw t182, -48($fp)
addi t181, t182, 1
li t183, 4
mul t180, t181, t183
add t177, t178, t180
move $at, t177
lw t184, 0($at)
beq t184, $zero, L6 
L7:
li $a0, 0
L8:
li t185, 1
beq t185, $a0, L11 
L12:
li $a3, 0
L13:
li t186, 1
beq t186, $a3, L14 
L15:
lw t187, -48($fp)
lw t190, 0($fp)
lw t189, -48(t190)
addi t188, t189, -1
bge t187, t188, L3 
L17:
lw t192, -48($fp)
addi t191, t192, 1
sw t191, -48($fp)
j L16 
L6:
li $v1, 1
lw t195, 0($fp)
lw t194, -60(t195)
lw t199, -48($fp)
add t198, t199, $zero
addi t197, t198, 1
li t200, 4
mul t196, t197, t200
add t193, t194, t196
move $v0, t193
lw t201, 0($v0)
beq t201, $zero, L4 
L5:
li $v1, 0
L4:
move $a0, $v1
j L8 
L11:
li $a2, 1
lw t204, 0($fp)
lw t203, -64(t204)
lw t209, -48($fp)
addi t208, t209, 7
sub t207, t208, $zero
addi t206, t207, 1
li t210, 4
mul t205, t206, t210
add t202, t203, t205
move $a1, t202
lw t211, 0($a1)
beq t211, $zero, L9 
L10:
li $a2, 0
L9:
move $a3, $a2
j L13 
L14:
lw t214, 0($fp)
lw t213, -52(t214)
lw t217, -48($fp)
addi t216, t217, 1
li t218, 4
mul t215, t216, t218
add t212, t213, t215
move $t0, t212
li t219, 1
sw t219, 0($t0)
lw t222, 0($fp)
lw t221, -60(t222)
lw t226, -48($fp)
add t225, t226, $zero
addi t224, t225, 1
li t227, 4
mul t223, t224, t227
add t220, t221, t223
move $t1, t220
li t228, 1
sw t228, 0($t1)
lw t231, 0($fp)
lw t230, -64(t231)
lw t236, -48($fp)
addi t235, t236, 7
sub t234, t235, $zero
addi t233, t234, 1
li t237, 4
mul t232, t233, t237
add t229, t230, t232
move $t2, t229
li t238, 1
sw t238, 0($t2)
lw t241, 0($fp)
lw t240, -56(t241)
addi t243, $zero, 1
li t244, 4
mul t242, t243, t244
add t239, t240, t242
move $t3, t239
lw t245, -48($fp)
sw t245, 0($t3)
lw t246, 0($fp)
move $a0, t246
addi t247, $zero, 1
move $a1, t247
jal L1
lw t250, 0($fp)
lw t249, -52(t250)
lw t253, -48($fp)
addi t252, t253, 1
li t254, 4
mul t251, t252, t254
add t248, t249, t251
move $t4, t248
sw $zero, 0($t4)
lw t257, 0($fp)
lw t256, -60(t257)
lw t261, -48($fp)
add t260, t261, $zero
addi t259, t260, 1
li t262, 4
mul t258, t259, t262
add t255, t256, t258
move $t5, t255
sw $zero, 0($t5)
lw t265, 0($fp)
lw t264, -64(t265)
lw t270, -48($fp)
addi t269, t270, 7
sub t268, t269, $zero
addi t267, t268, 1
li t271, 4
mul t266, t267, t271
add t263, t264, t266
move $t6, t263
sw $zero, 0($t6)
j L15 
L40:

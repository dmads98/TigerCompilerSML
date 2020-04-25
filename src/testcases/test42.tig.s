.data
.align 4
L9: .asciiz "sdf"
L8: .asciiz "sfd"
L7: .asciiz "kati"
L6: .asciiz "Allos"
L5: .asciiz "Kapou"
L4: .asciiz "Kapoios"
L3: .asciiz ""
L2: .asciiz "somewhere"
L1: .asciiz "aname"
.text
.globl tig_main
.ent tig_main
#-----------tig_main----------
L11:
sw $a0, 0($fp)
li $a0, 100
la $a1, L3
jal tig_initArray
move t143, $v0
li $a0, 10
li $a1, 0
jal tig_initArray
move t140, $v0
li $a0, 16
jal tig_allocRecord
move t141, $v0
sw $zero, 0(t141)
sw $zero, 4(t141)
la t155, L2
sw t155, 8(t141)
la t156, L1
sw t156, 12(t141)
li $a0, 5
move $a1, t141
jal tig_initArray
move t142, $v0
li $a0, 16
jal tig_allocRecord
move t144, $v0
li t157, 44
sw t157, 0(t144)
li t158, 2432
sw t158, 4(t144)
la t159, L5
sw t159, 8(t144)
la t160, L4
sw t160, 12(t144)
move t145, t144
li $a0, 8
jal tig_allocRecord
move t146, $v0
addi t161, t146, 0
move t154, t161
li $a0, 3
li $a1, 1900
jal tig_initArray
move t153, $v0
sw t153, 0(t154)
la t162, L6
sw t162, 4(t146)
move t147, t146
li t163, 1
sw t163, 0(t140)
li t167, 9
addi t166, t167, 1
li t168, 4
mul t165, t166, t168
add t164, t140, t165
move t148, t164
li t169, 3
sw t169, 0(t148)
li t173, 3
addi t172, t173, 1
li t174, 4
mul t171, t172, t174
add t170, t142, t171
move t149, t170
la t175, L7
lw t176, 0(t149)
sw t175, 12(t176)
li t180, 1
addi t179, t180, 1
li t181, 4
mul t178, t179, t181
add t177, t142, t178
move t150, t177
li t182, 23
lw t183, 0(t150)
sw t182, 0(t183)
li t187, 34
addi t186, t187, 1
li t188, 4
mul t185, t186, t188
add t184, t143, t185
move t151, t184
la t189, L8
sw t189, 0(t151)
la t190, L9
sw t190, 12(t145)
li t191, 2323
lw t192, 0(t147)
sw t191, 0(t192)
lw t194, 0(t147)
li t197, 2
addi t196, t197, 1
li t198, 4
mul t195, t196, t198
add t193, t194, t195
move t152, t193
li t199, 2323
sw t199, 0(t152)
li $v0, 0
j L10 
L10:

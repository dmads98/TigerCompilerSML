L26:

L20: .
L19: O
L99:
li t193, 0
move t145, t193
lw t196, 0('s0)
lw t195, -4('s0)
li t197, 1
sub t194, t195, t197
move t151, t194
ble t145, t151, L28 
L14:
addi t198, $sp, 0
move $sp, t198
la t199, L26
move $a0, t199
jal L5
addi t200, $sp, 0
move $sp, t200
move $v0, $v0
j L98 
L27:
addi t201, t145, 1
move t145, t201
L28:
li t202, 0
move t146, t202
lw t205, 0('s0)
lw t204, -4('s0)
li t206, 1
sub t203, t204, t206
move t150, t203
ble t146, t150, L25 
L15:
addi t207, $sp, 0
move $sp, t207
la t208, L26
move $a0, t208
jal L5
addi t209, $sp, 0
move $sp, t209
blt t145, t151, L27 
L100:
j L14 
L24:
addi t210, t146, 1
move t146, t210
L25:
move t147, t145
lw t212, 0('s0)
lw t211, -12('s0)
move t148, t211
lw t213, 0(t148)
bge t147, t213, L16 
L18:
li t214, 0
blt t147, t214, L16 
L17:
addi t218, t147, 1
li t219, 4
mul t217, t218, t219
add t216, t148, t217
lw t215, 0(t216)
beq t215, t146, L21 
L22:
la t220, L20
move t149, t220
L23:
addi t221, $sp, 0
move $sp, t221
move $a0, t149
jal L5
addi t222, $sp, 0
move $sp, t222
blt t146, t150, L24 
L101:
j L15 
L16:
addi t223, $sp, 0
move $sp, t223
li t224, 1
move $a0, t224
jal exit
addi t225, $sp, 0
move $sp, t225
j L17 
L21:
la t226, L19
move t149, t226
j L23 
L98:
L103:
lw t228, 0('s0)
lw t227, -4('s0)
beq t144, t227, L95 
L96:
li t229, 0
move t152, t229
lw t232, 0('s0)
lw t231, -4('s0)
li t233, 1
sub t230, t231, t233
move t191, t230
ble t152, t191, L94 
L29:
li t234, 0
move t192, t234
L97:
move $v0, t192
j L102 
L95:
addi t235, $sp, 0
move $sp, t235
lw t236, 0($fp)
move $a0, t236
jal L12
addi t237, $sp, 0
move $sp, t237
move t192, $v0
j L97 
L93:
addi t238, t152, 1
move t152, t238
L94:
move t153, t152
lw t240, 0('s0)
lw t239, -8('s0)
move t154, t239
lw t241, 0(t154)
bge t153, t241, L30 
L32:
li t242, 0
blt t153, t242, L30 
L31:
addi t246, t153, 1
li t247, 4
mul t245, t246, t247
add t244, t154, t245
lw t243, 0(t244)
li t248, 0
beq t243, t248, L38 
L39:
li t249, 0
move t158, t249
L40:
li t250, 0
beq t158, t250, L47 
L46:
li t251, 1
move t161, t251
addi t253, t152, 7
sub t252, t253, t144
move t159, t252
lw t255, 0('s0)
lw t254, -20('s0)
move t160, t254
lw t256, 0(t160)
bge t159, t256, L41 
L43:
li t257, 0
blt t159, t257, L41 
L42:
addi t261, t159, 1
li t262, 4
mul t260, t261, t262
add t259, t160, t260
lw t258, 0(t259)
li t263, 0
beq t258, t263, L44 
L45:
li t264, 0
move t161, t264
L44:
move t162, t161
L48:
li t265, 0
beq t162, t265, L92 
L91:
move t163, t152
lw t267, 0('s0)
lw t266, -8('s0)
move t164, t266
lw t268, 0(t164)
bge t163, t268, L49 
L51:
li t269, 0
blt t163, t269, L49 
L50:
li t270, 1
addi t273, t163, 1
li t274, 4
mul t272, t273, t274
add t271, t164, t272
sw t270, 0(t271) 
 add t275, t152, t144
move t165, t275
lw t277, 0('s0)
lw t276, -16('s0)
move t166, t276
lw t278, 0(t166)
bge t165, t278, L52 
L54:
li t279, 0
blt t165, t279, L52 
L53:
li t280, 1
addi t283, t165, 1
li t284, 4
mul t282, t283, t284
add t281, t166, t282
sw t280, 0(t281) 
 addi t286, t152, 7
sub t285, t286, t144
move t167, t285
lw t288, 0('s0)
lw t287, -20('s0)
move t168, t287
lw t289, 0(t168)
bge t167, t289, L55 
L57:
li t290, 0
blt t167, t290, L55 
L56:
li t291, 1
addi t294, t167, 1
li t295, 4
mul t293, t294, t295
add t292, t168, t293
sw t291, 0(t292) 
 move t169, t144
lw t297, 0('s0)
lw t296, -12('s0)
move t170, t296
lw t298, 0(t170)
bge t169, t298, L58 
L60:
li t299, 0
blt t169, t299, L58 
L59:
addi t302, t169, 1
li t303, 4
mul t301, t302, t303
add t300, t170, t301
sw t152, 0(t300) 
 addi t304, $sp, 0
move $sp, t304
lw t305, 0($fp)
move $a0, t305
addi t306, t144, 1
move $a1, t306
jal L13
addi t307, $sp, 0
move $sp, t307
move t171, t152
lw t309, 0('s0)
lw t308, -8('s0)
move t172, t308
lw t310, 0(t172)
bge t171, t310, L61 
L63:
li t311, 0
blt t171, t311, L61 
L62:
li t312, 0
addi t315, t171, 1
li t316, 4
mul t314, t315, t316
add t313, t172, t314
sw t312, 0(t313) 
 add t317, t152, t144
move t173, t317
lw t319, 0('s0)
lw t318, -16('s0)
move t174, t318
lw t320, 0(t174)
bge t173, t320, L64 
L66:
li t321, 0
blt t173, t321, L64 
L65:
li t322, 0
addi t325, t173, 1
li t326, 4
mul t324, t325, t326
add t323, t174, t324
sw t322, 0(t323) 
 addi t328, t152, 7
sub t327, t328, t144
move t175, t327
lw t330, 0('s0)
lw t329, -20('s0)
move t176, t329
lw t331, 0(t176)
bge t175, t331, L67 
L69:
li t332, 0
blt t175, t332, L67 
L68:
li t333, 0
addi t336, t175, 1
li t337, 4
mul t335, t336, t337
add t334, t176, t335
sw t333, 0(t334) 
 L92:
blt t152, t191, L93 
L104:
j L29 
L30:
addi t338, $sp, 0
move $sp, t338
li t339, 1
move $a0, t339
jal exit
addi t340, $sp, 0
move $sp, t340
j L31 
L38:
li t341, 1
move t157, t341
add t342, t152, t144
move t155, t342
lw t344, 0('s0)
lw t343, -16('s0)
move t156, t343
lw t345, 0(t156)
bge t155, t345, L33 
L35:
li t346, 0
blt t155, t346, L33 
L34:
addi t350, t155, 1
li t351, 4
mul t349, t350, t351
add t348, t156, t349
lw t347, 0(t348)
li t352, 0
beq t347, t352, L36 
L37:
li t353, 0
move t157, t353
L36:
move t158, t157
j L40 
L33:
addi t354, $sp, 0
move $sp, t354
li t355, 1
move $a0, t355
jal exit
addi t356, $sp, 0
move $sp, t356
j L34 
L41:
addi t357, $sp, 0
move $sp, t357
li t358, 1
move $a0, t358
jal exit
addi t359, $sp, 0
move $sp, t359
j L42 
L47:
li t360, 0
move t162, t360
j L48 
L49:
addi t361, $sp, 0
move $sp, t361
li t362, 1
move $a0, t362
jal exit
addi t363, $sp, 0
move $sp, t363
j L50 
L52:
addi t364, $sp, 0
move $sp, t364
li t365, 1
move $a0, t365
jal exit
addi t366, $sp, 0
move $sp, t366
j L53 
L55:
addi t367, $sp, 0
move $sp, t367
li t368, 1
move $a0, t368
jal exit
addi t369, $sp, 0
move $sp, t369
j L56 
L58:
addi t370, $sp, 0
move $sp, t370
li t371, 1
move $a0, t371
jal exit
addi t372, $sp, 0
move $sp, t372
j L59 
L61:
addi t373, $sp, 0
move $sp, t373
li t374, 1
move $a0, t374
jal exit
addi t375, $sp, 0
move $sp, t375
j L62 
L64:
addi t376, $sp, 0
move $sp, t376
li t377, 1
move $a0, t377
jal exit
addi t378, $sp, 0
move $sp, t378
j L65 
L67:
addi t379, $sp, 0
move $sp, t379
li t380, 1
move $a0, t380
jal exit
addi t381, $sp, 0
move $sp, t381
j L68 
L102:
L106:
li t386, 8
sw 's0, -4($fp) 
addi t387, $fp, -8
move t382, t387
addi t388, $sp, 0
move $sp, t388
lw t390, -4('s0)
addi t389, t390, 1
move $a0, t389
li t391, 0
move $a1, t391
jal initArray
addi t392, $sp, 0
move $sp, t392
move t140, $v0
lw t393, -4('s0)
sw t393, 0(t140) 
 sw t140, 0(t382) 
 addi t394, $fp, -12
move t383, t394
addi t395, $sp, 0
move $sp, t395
lw t397, -4('s0)
addi t396, t397, 1
move $a0, t396
li t398, 0
move $a1, t398
jal initArray
addi t399, $sp, 0
move $sp, t399
move t141, $v0
lw t400, -4('s0)
sw t400, 0(t141) 
 sw t141, 0(t383) 
 addi t401, $fp, -16
move t384, t401
addi t402, $sp, 0
move $sp, t402
lw t406, -4('s0)
lw t407, -4('s0)
add t405, t406, t407
li t408, 1
sub t404, t405, t408
addi t403, t404, 1
move $a0, t403
li t409, 0
move $a1, t409
jal initArray
addi t410, $sp, 0
move $sp, t410
move t142, $v0
lw t413, -4('s0)
lw t414, -4('s0)
add t412, t413, t414
li t415, 1
sub t411, t412, t415
sw t411, 0(t142) 
 sw t142, 0(t384) 
 addi t416, $fp, -20
move t385, t416
addi t417, $sp, 0
move $sp, t417
lw t421, -4('s0)
lw t422, -4('s0)
add t420, t421, t422
li t423, 1
sub t419, t420, t423
addi t418, t419, 1
move $a0, t418
li t424, 0
move $a1, t424
jal initArray
addi t425, $sp, 0
move $sp, t425
move t143, $v0
lw t428, -4('s0)
lw t429, -4('s0)
add t427, t428, t429
li t430, 1
sub t426, t427, t430
sw t426, 0(t143) 
 sw t143, 0(t385) 
 addi t431, $sp, 0
move $sp, t431
move $a0, $fp
li t432, 0
move $a1, t432
jal L13
addi t433, $sp, 0
move $sp, t433
move $v0, $v0
j L105 
L105:

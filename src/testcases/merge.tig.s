L50:-
L22:

L21: 
L14:9
L13:0
L61:
addi t164, $sp, 0
move $sp, t164
lw t167, 0('s0)
lw t166, 0('s0)
lw t165, -4('s0)
move $a0, t165
jal L1
addi t168, $sp, 0
move $sp, t168
move t158, $v0
move t160, t158
addi t169, $sp, 0
move $sp, t169
la t170, L13
move $a0, t170
jal L1
addi t171, $sp, 0
move $sp, t171
move t159, $v0
bge t160, t159, L17 
L18:
li t172, 0
move t136, t172
L19:
move $v0, t136
j L60 
L17:
li t173, 1
move t135, t173
addi t174, $sp, 0
move $sp, t174
lw t177, 0('s0)
lw t176, 0('s0)
lw t175, -4('s0)
move $a0, t175
jal L1
addi t178, $sp, 0
move $sp, t178
move t161, $v0
move t163, t161
addi t179, $sp, 0
move $sp, t179
la t180, L14
move $a0, t180
jal L1
addi t181, $sp, 0
move $sp, t181
move t162, $v0
ble t163, t162, L15 
L16:
li t182, 0
move t135, t182
L15:
move t136, t135
j L19 
L60:
L63:
L26:
addi t186, $sp, 0
move $sp, t186
lw t189, 0('s0)
lw t188, 0('s0)
lw t187, -4('s0)
move $a0, t187
la t190, L21
move $a1, t190
jal stringEqual
addi t191, $sp, 0
move $sp, t191
move t185, $v0
li t192, 0
beq t185, t192, L24 
L23:
li t193, 1
move t137, t193
L25:
li t194, 0
beq t137, t194, L20 
L27:
lw t197, 0('s0)
lw t196, 0('s0)
addi t195, t196, -4
move t184, t195
addi t198, $sp, 0
move $sp, t198
jal L9
addi t199, $sp, 0
move $sp, t199
move t183, $v0
sw t183, 0(t184) 
j L26 
L24:
addi t200, $sp, 0
move $sp, t200
lw t203, 0('s0)
lw t202, 0('s0)
lw t201, -4('s0)
move $a0, t201
la t204, L22
move $a1, t204
jal stringEqual
addi t205, $sp, 0
move $sp, t205
move t137, $v0
j L25 
L20:
li t206, 0
move $v0, t206
j L62 
L62:
L65:
li t216, 0
move t133, t216
addi t217, $sp, 0
move $sp, t217
move $a0, $fp
jal L12
addi t218, $sp, 0
move $sp, t218
addi t219, t132, 0
move t208, t219
addi t220, $sp, 0
move $sp, t220
move $a0, $fp
lw t222, 0('s0)
lw t221, -4('s0)
move $a1, t221
jal L11
addi t223, $sp, 0
move $sp, t223
move t207, $v0
sw t207, 0(t208) 
L29:
addi t224, $sp, 0
move $sp, t224
move $a0, $fp
lw t226, 0('s0)
lw t225, -4('s0)
move $a1, t225
jal L11
addi t227, $sp, 0
move $sp, t227
move t215, $v0
li t228, 0
beq t215, t228, L28 
L30:
li t230, 10
mul t229, t133, t230
move t210, t229
addi t231, $sp, 0
move $sp, t231
lw t233, 0('s0)
lw t232, -4('s0)
move $a0, t232
jal L1
addi t234, $sp, 0
move $sp, t234
move t209, $v0
add t235, t210, t209
move t212, t235
addi t236, $sp, 0
move $sp, t236
la t237, L13
move $a0, t237
jal L1
addi t238, $sp, 0
move $sp, t238
move t211, $v0
sub t239, t212, t211
move t133, t239
lw t241, 0('s0)
addi t240, t241, -4
move t214, t240
addi t242, $sp, 0
move $sp, t242
jal L9
addi t243, $sp, 0
move $sp, t243
move t213, $v0
sw t213, 0(t214) 
j L29 
L28:
move $v0, t133
j L64 
L64:
L67:
addi t246, $sp, 0
move $sp, t246
li t247, 1
move $a0, t247
jal allocRecord
addi t248, $sp, 0
move $sp, t248
move t142, $v0
li t249, 0
sw 's0, 0(t142) 
move t143, t142
addi t250, $sp, 0
move $sp, t250
lw t251, 0($fp)
move $a0, t251
move $a1, t143
jal L10
addi t252, $sp, 0
move $sp, t252
move t144, $v0
lw t253, 0('s0)
li t254, 0
beq t253, t254, L36 
L35:
addi t255, $sp, 0
move $sp, t255
li t256, 2
move $a0, t256
jal allocRecord
addi t257, $sp, 0
move $sp, t257
move t145, $v0
sw 's0, 0(t145) 
addi t258, t145, 4
move t245, t258
addi t259, $sp, 0
move $sp, t259
lw t260, 0($fp)
move $a0, t260
jal L31
addi t261, $sp, 0
move $sp, t261
move t244, $v0
sw t244, 0(t245) 
move t146, t145
L37:
move $v0, t146
j L66 
L36:
li t262, 0
move t146, t262
j L37 
L66:
L69:
li t267, 0
beq t138, t267, L44 
L45:
li t268, 0
beq t139, t268, L41 
L42:
lw t269, 0('s0)
lw t270, 0('s0)
blt t269, t270, L38 
L39:
addi t271, $sp, 0
move $sp, t271
li t272, 2
move $a0, t272
jal allocRecord
addi t273, $sp, 0
move $sp, t273
move t148, $v0
lw t274, 0('s0)
sw 's0, 0(t148) 
addi t275, t148, 4
move t266, t275
addi t276, $sp, 0
move $sp, t276
lw t277, 0($fp)
move $a0, t277
move $a1, t138
lw t278, 4('s0)
move $a2, t278
jal L32
addi t279, $sp, 0
move $sp, t279
move t265, $v0
sw t265, 0(t266) 
move t149, t148
L40:
move t150, t149
L43:
move t151, t150
L46:
move $v0, t151
j L68 
L44:
move t151, t139
j L46 
L41:
move t150, t138
j L43 
L38:
addi t280, $sp, 0
move $sp, t280
li t281, 2
move $a0, t281
jal allocRecord
addi t282, $sp, 0
move $sp, t282
move t147, $v0
lw t283, 0('s0)
sw 's0, 0(t147) 
addi t284, t147, 4
move t264, t284
addi t285, $sp, 0
move $sp, t285
lw t286, 0($fp)
move $a0, t286
lw t287, 4('s0)
move $a1, t287
move $a2, t139
jal L32
addi t288, $sp, 0
move $sp, t288
move t263, $v0
sw t263, 0(t264) 
move t149, t147
j L40 
L68:
L71:
li t292, 0
bgt t152, t292, L48 
L49:
li t293, 0
move $v0, t293
j L70 
L48:
addi t294, $sp, 0
move $sp, t294
lw t295, 0($fp)
move $a0, t295
li t297, 10
div t296, t152, t297
move $a1, t296
jal L47
addi t298, $sp, 0
move $sp, t298
li t302, 10
div t301, t152, t302
li t303, 10
mul t300, t301, t303
sub t299, t152, t300
move t291, t299
addi t304, $sp, 0
move $sp, t304
la t305, L13
move $a0, t305
jal L1
addi t306, $sp, 0
move $sp, t306
move t290, $v0
addi t307, $sp, 0
move $sp, t307
add t308, t291, t290
move $a0, t308
jal L2
addi t309, $sp, 0
move $sp, t309
move t289, $v0
addi t310, $sp, 0
move $sp, t310
move $a0, t289
jal L5
addi t311, $sp, 0
move $sp, t311
j L49 
L70:
L73:
li t312, 0
blt t140, t312, L54 
L55:
li t313, 0
bgt t140, t313, L51 
L52:
addi t314, $sp, 0
move $sp, t314
la t315, L13
move $a0, t315
jal L5
addi t316, $sp, 0
move $sp, t316
move t153, $v0
L53:
move t154, t153
L56:
move $v0, t154
j L72 
L54:
addi t317, $sp, 0
move $sp, t317
la t318, L50
move $a0, t318
jal L5
addi t319, $sp, 0
move $sp, t319
addi t320, $sp, 0
move $sp, t320
move $a0, $fp
li t322, 0
sub t321, t322, t140
move $a1, t321
jal L47
addi t323, $sp, 0
move $sp, t323
move t154, $v0
j L56 
L51:
addi t324, $sp, 0
move $sp, t324
move $a0, $fp
move $a1, t140
jal L47
addi t325, $sp, 0
move $sp, t325
move t153, $v0
j L53 
L72:
L75:
li t326, 0
beq t141, t326, L57 
L58:
addi t327, $sp, 0
move $sp, t327
lw t328, 0($fp)
move $a0, t328
lw t329, 0('s0)
move $a1, t329
jal L33
addi t330, $sp, 0
move $sp, t330
addi t331, $sp, 0
move $sp, t331
la t332, L21
move $a0, t332
jal L5
addi t333, $sp, 0
move $sp, t333
addi t334, $sp, 0
move $sp, t334
lw t335, 0($fp)
move $a0, t335
lw t336, 4('s0)
move $a1, t336
jal L34
addi t337, $sp, 0
move $sp, t337
move t155, $v0
L59:
move $v0, t155
j L74 
L57:
addi t338, $sp, 0
move $sp, t338
la t339, L22
move $a0, t339
jal L5
addi t340, $sp, 0
move $sp, t340
move t155, $v0
j L59 
L74:
L77:
addi t347, $fp, -4
move t342, t347
addi t348, $sp, 0
move $sp, t348
jal L9
addi t349, $sp, 0
move $sp, t349
move t341, $v0
sw t341, 0(t342) 
addi t350, $sp, 0
move $sp, t350
move $a0, $fp
jal L31
addi t351, $sp, 0
move $sp, t351
move t156, $v0
addi t352, $fp, -4
move t344, t352
addi t353, $sp, 0
move $sp, t353
jal L9
addi t354, $sp, 0
move $sp, t354
move t343, $v0
sw t343, 0(t344) 
addi t355, $sp, 0
move $sp, t355
move $a0, $fp
jal L31
addi t356, $sp, 0
move $sp, t356
move t157, $v0
move t346, $fp
addi t357, $sp, 0
move $sp, t357
move $a0, $fp
move $a1, t156
move $a2, t157
jal L32
addi t358, $sp, 0
move $sp, t358
move t345, $v0
addi t359, $sp, 0
move $sp, t359
move $a0, t346
move $a1, t345
jal L34
addi t360, $sp, 0
move $sp, t360
move $v0, $v0
j L76 
L76:

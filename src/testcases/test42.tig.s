L108:sdf
L107:sfd
L100:kati
L90:Allos
L89:Kapou
L88:Kapoios
L87:
L86:somewhere
L85:aname
L116:
addi t300, $sp, 0
move $sp, t300
li t302, 10
addi t301, t302, 1
move $a0, t301
li t303, 0
move $a1, t303
jal initArray
addi t304, $sp, 0
move $sp, t304
move t272, $v0
li t305, 10
sw t305, 0(t272) 
move t273, t272
li t307, 5
addi t306, t307, 1
move t298, t306
addi t308, $sp, 0
move $sp, t308
li t309, 4
move $a0, t309
jal allocRecord
addi t310, $sp, 0
move $sp, t310
move t274, $v0
la t311, L85
sw 's0, 0(t274) 
la t312, L86
sw 's0, 4(t274) 
li t313, 0
sw 's0, 8(t274) 
li t314, 0
sw 's0, 12(t274) 
addi t315, $sp, 0
move $sp, t315
move $a0, t298
move $a1, t274
jal initArray
addi t316, $sp, 0
move $sp, t316
move t275, $v0
li t317, 5
sw t317, 0(t275) 
move t276, t275
addi t318, $sp, 0
move $sp, t318
li t320, 100
addi t319, t320, 1
move $a0, t319
la t321, L87
move $a1, t321
jal initArray
addi t322, $sp, 0
move $sp, t322
move t277, $v0
li t323, 100
sw t323, 0(t277) 
move t278, t277
addi t324, $sp, 0
move $sp, t324
li t325, 4
move $a0, t325
jal allocRecord
addi t326, $sp, 0
move $sp, t326
move t279, $v0
la t327, L88
sw 's0, 0(t279) 
la t328, L89
sw 's0, 4(t279) 
li t329, 2432
sw 's0, 8(t279) 
li t330, 44
sw 's0, 12(t279) 
move t280, t279
addi t331, $sp, 0
move $sp, t331
li t332, 2
move $a0, t332
jal allocRecord
addi t333, $sp, 0
move $sp, t333
move t282, $v0
la t334, L90
sw 's0, 0(t282) 
addi t335, t282, 4
move t299, t335
addi t336, $sp, 0
move $sp, t336
li t338, 3
addi t337, t338, 1
move $a0, t337
li t339, 1900
move $a1, t339
jal initArray
addi t340, $sp, 0
move $sp, t340
move t281, $v0
li t341, 3
sw t341, 0(t281) 
sw t281, 0(t299) 
move t283, t282
li t342, 0
move t284, t342
move t285, t273
lw t343, 0(t285)
bge t284, t343, L91 
L93:
li t344, 0
blt t284, t344, L91 
L92:
li t345, 1
addi t348, t284, 1
li t349, 4
mul t347, t348, t349
add t346, t285, t347
sw t345, 0(t346) 
li t350, 9
move t286, t350
move t287, t273
lw t351, 0(t287)
bge t286, t351, L94 
L96:
li t352, 0
blt t286, t352, L94 
L95:
li t353, 3
addi t356, t286, 1
li t357, 4
mul t355, t356, t357
add t354, t287, t355
sw t353, 0(t354) 
li t358, 3
move t288, t358
move t289, t276
lw t359, 0(t289)
bge t288, t359, L97 
L99:
li t360, 0
blt t288, t360, L97 
L98:
la t361, L100
addi t365, t288, 1
li t366, 4
mul t364, t365, t366
add t363, t289, t364
lw t362, 0(t363)
sw 's0, 0(t362) 
li t367, 1
move t290, t367
move t291, t276
lw t368, 0(t291)
bge t290, t368, L101 
L103:
li t369, 0
blt t290, t369, L101 
L102:
li t370, 23
addi t374, t290, 1
li t375, 4
mul t373, t374, t375
add t372, t291, t373
lw t371, 0(t372)
sw 's0, 12(t371) 
li t376, 34
move t292, t376
move t293, t278
lw t377, 0(t293)
bge t292, t377, L104 
L106:
li t378, 0
blt t292, t378, L104 
L105:
la t379, L107
addi t382, t292, 1
li t383, 4
mul t381, t382, t383
add t380, t293, t381
sw t379, 0(t380) 
la t384, L108
sw 's0, 0(t280) 
li t385, 0
move t294, t385
lw t386, 4('s0)
move t295, t386
lw t387, 0(t295)
bge t294, t387, L109 
L111:
li t388, 0
blt t294, t388, L109 
L110:
li t389, 2323
addi t392, t294, 1
li t393, 4
mul t391, t392, t393
add t390, t295, t391
sw t389, 0(t390) 
li t394, 2
move t296, t394
lw t395, 4('s0)
move t297, t395
lw t396, 0(t297)
bge t296, t396, L112 
L114:
li t397, 0
blt t296, t397, L112 
L113:
li t398, 2323
addi t401, t296, 1
li t402, 4
mul t400, t401, t402
add t399, t297, t400
sw t398, 0(t399) 
li t403, 0
move $v0, t403
j L115 
L91:
addi t404, $sp, 0
move $sp, t404
li t405, 1
move $a0, t405
jal exit
addi t406, $sp, 0
move $sp, t406
j L92 
L94:
addi t407, $sp, 0
move $sp, t407
li t408, 1
move $a0, t408
jal exit
addi t409, $sp, 0
move $sp, t409
j L95 
L97:
addi t410, $sp, 0
move $sp, t410
li t411, 1
move $a0, t411
jal exit
addi t412, $sp, 0
move $sp, t412
j L98 
L101:
addi t413, $sp, 0
move $sp, t413
li t414, 1
move $a0, t414
jal exit
addi t415, $sp, 0
move $sp, t415
j L102 
L104:
addi t416, $sp, 0
move $sp, t416
li t417, 1
move $a0, t417
jal exit
addi t418, $sp, 0
move $sp, t418
j L105 
L109:
addi t419, $sp, 0
move $sp, t419
li t420, 1
move $a0, t420
jal exit
addi t421, $sp, 0
move $sp, t421
j L110 
L112:
addi t422, $sp, 0
move $sp, t422
li t423, 1
move $a0, t423
jal exit
addi t424, $sp, 0
move $sp, t424
j L113 
L115:

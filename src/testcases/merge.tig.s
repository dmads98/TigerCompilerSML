L269:-
L241:

L240: 
L233:9
L232:0
L280:
addi t767, $sp, 0
move $sp, t767
lw t770, 0('s0)
lw t769, 0('s0)
lw t768, -4('s0)
move $a0, t768
jal L1
addi t771, $sp, 0
move $sp, t771
move t761, $v0
move t763, t761
addi t772, $sp, 0
move $sp, t772
la t773, L232
move $a0, t773
jal L1
addi t774, $sp, 0
move $sp, t774
move t762, $v0
bge t763, t762, L236 
L237:
li t775, 0
move t739, t775
L238:
move $v0, t739
j L279 
L236:
li t776, 1
move t738, t776
addi t777, $sp, 0
move $sp, t777
lw t780, 0('s0)
lw t779, 0('s0)
lw t778, -4('s0)
move $a0, t778
jal L1
addi t781, $sp, 0
move $sp, t781
move t764, $v0
move t766, t764
addi t782, $sp, 0
move $sp, t782
la t783, L233
move $a0, t783
jal L1
addi t784, $sp, 0
move $sp, t784
move t765, $v0
ble t766, t765, L234 
L235:
li t785, 0
move t738, t785
L234:
move t739, t738
j L238 
L279:
L282:
L245:
addi t789, $sp, 0
move $sp, t789
lw t792, 0('s0)
lw t791, 0('s0)
lw t790, -4('s0)
move $a0, t790
la t793, L240
move $a1, t793
jal stringEqual
addi t794, $sp, 0
move $sp, t794
move t788, $v0
li t795, 0
beq t788, t795, L243 
L242:
li t796, 1
move t740, t796
L244:
li t797, 0
beq t740, t797, L239 
L246:
lw t800, 0('s0)
lw t799, 0('s0)
addi t798, t799, -4
move t787, t798
addi t801, $sp, 0
move $sp, t801
jal L9
addi t802, $sp, 0
move $sp, t802
move t786, $v0
sw t786, 0(t787) 
j L245 
L243:
addi t803, $sp, 0
move $sp, t803
lw t806, 0('s0)
lw t805, 0('s0)
lw t804, -4('s0)
move $a0, t804
la t807, L241
move $a1, t807
jal stringEqual
addi t808, $sp, 0
move $sp, t808
move t740, $v0
j L244 
L239:
li t809, 0
move $v0, t809
j L281 
L281:
L284:
li t819, 0
move t736, t819
addi t820, $sp, 0
move $sp, t820
move $a0, $fp
jal L231
addi t821, $sp, 0
move $sp, t821
addi t822, t735, 0
move t811, t822
addi t823, $sp, 0
move $sp, t823
move $a0, $fp
lw t825, 0('s0)
lw t824, -4('s0)
move $a1, t824
jal L230
addi t826, $sp, 0
move $sp, t826
move t810, $v0
sw t810, 0(t811) 
L248:
addi t827, $sp, 0
move $sp, t827
move $a0, $fp
lw t829, 0('s0)
lw t828, -4('s0)
move $a1, t828
jal L230
addi t830, $sp, 0
move $sp, t830
move t818, $v0
li t831, 0
beq t818, t831, L247 
L249:
li t833, 10
mul t832, t736, t833
move t813, t832
addi t834, $sp, 0
move $sp, t834
lw t836, 0('s0)
lw t835, -4('s0)
move $a0, t835
jal L1
addi t837, $sp, 0
move $sp, t837
move t812, $v0
add t838, t813, t812
move t815, t838
addi t839, $sp, 0
move $sp, t839
la t840, L232
move $a0, t840
jal L1
addi t841, $sp, 0
move $sp, t841
move t814, $v0
sub t842, t815, t814
move t736, t842
lw t844, 0('s0)
addi t843, t844, -4
move t817, t843
addi t845, $sp, 0
move $sp, t845
jal L9
addi t846, $sp, 0
move $sp, t846
move t816, $v0
sw t816, 0(t817) 
j L248 
L247:
move $v0, t736
j L283 
L283:
L286:
addi t849, $sp, 0
move $sp, t849
li t850, 1
move $a0, t850
jal allocRecord
addi t851, $sp, 0
move $sp, t851
move t745, $v0
li t852, 0
sw 's0, 0(t745) 
move t746, t745
addi t853, $sp, 0
move $sp, t853
lw t854, 0($fp)
move $a0, t854
move $a1, t746
jal L229
addi t855, $sp, 0
move $sp, t855
move t747, $v0
lw t856, 0('s0)
li t857, 0
beq t856, t857, L255 
L254:
addi t858, $sp, 0
move $sp, t858
li t859, 2
move $a0, t859
jal allocRecord
addi t860, $sp, 0
move $sp, t860
move t748, $v0
sw 's0, 0(t748) 
addi t861, t748, 4
move t848, t861
addi t862, $sp, 0
move $sp, t862
lw t863, 0($fp)
move $a0, t863
jal L250
addi t864, $sp, 0
move $sp, t864
move t847, $v0
sw t847, 0(t848) 
move t749, t748
L256:
move $v0, t749
j L285 
L255:
li t865, 0
move t749, t865
j L256 
L285:
L288:
li t870, 0
beq t741, t870, L263 
L264:
li t871, 0
beq t742, t871, L260 
L261:
lw t872, 0('s0)
lw t873, 0('s0)
blt t872, t873, L257 
L258:
addi t874, $sp, 0
move $sp, t874
li t875, 2
move $a0, t875
jal allocRecord
addi t876, $sp, 0
move $sp, t876
move t751, $v0
lw t877, 0('s0)
sw 's0, 0(t751) 
addi t878, t751, 4
move t869, t878
addi t879, $sp, 0
move $sp, t879
lw t880, 0($fp)
move $a0, t880
move $a1, t741
lw t881, 4('s0)
move $a2, t881
jal L251
addi t882, $sp, 0
move $sp, t882
move t868, $v0
sw t868, 0(t869) 
move t752, t751
L259:
move t753, t752
L262:
move t754, t753
L265:
move $v0, t754
j L287 
L263:
move t754, t742
j L265 
L260:
move t753, t741
j L262 
L257:
addi t883, $sp, 0
move $sp, t883
li t884, 2
move $a0, t884
jal allocRecord
addi t885, $sp, 0
move $sp, t885
move t750, $v0
lw t886, 0('s0)
sw 's0, 0(t750) 
addi t887, t750, 4
move t867, t887
addi t888, $sp, 0
move $sp, t888
lw t889, 0($fp)
move $a0, t889
lw t890, 4('s0)
move $a1, t890
move $a2, t742
jal L251
addi t891, $sp, 0
move $sp, t891
move t866, $v0
sw t866, 0(t867) 
move t752, t750
j L259 
L287:
L290:
li t895, 0
bgt t755, t895, L267 
L268:
li t896, 0
move $v0, t896
j L289 
L267:
addi t897, $sp, 0
move $sp, t897
lw t898, 0($fp)
move $a0, t898
li t900, 10
div t899, t755, t900
move $a1, t899
jal L266
addi t901, $sp, 0
move $sp, t901
li t905, 10
div t904, t755, t905
li t906, 10
mul t903, t904, t906
sub t902, t755, t903
move t894, t902
addi t907, $sp, 0
move $sp, t907
la t908, L232
move $a0, t908
jal L1
addi t909, $sp, 0
move $sp, t909
move t893, $v0
addi t910, $sp, 0
move $sp, t910
add t911, t894, t893
move $a0, t911
jal L2
addi t912, $sp, 0
move $sp, t912
move t892, $v0
addi t913, $sp, 0
move $sp, t913
move $a0, t892
jal L5
addi t914, $sp, 0
move $sp, t914
j L268 
L289:
L292:
li t915, 0
blt t743, t915, L273 
L274:
li t916, 0
bgt t743, t916, L270 
L271:
addi t917, $sp, 0
move $sp, t917
la t918, L232
move $a0, t918
jal L5
addi t919, $sp, 0
move $sp, t919
move t756, $v0
L272:
move t757, t756
L275:
move $v0, t757
j L291 
L273:
addi t920, $sp, 0
move $sp, t920
la t921, L269
move $a0, t921
jal L5
addi t922, $sp, 0
move $sp, t922
addi t923, $sp, 0
move $sp, t923
move $a0, $fp
li t925, 0
sub t924, t925, t743
move $a1, t924
jal L266
addi t926, $sp, 0
move $sp, t926
move t757, $v0
j L275 
L270:
addi t927, $sp, 0
move $sp, t927
move $a0, $fp
move $a1, t743
jal L266
addi t928, $sp, 0
move $sp, t928
move t756, $v0
j L272 
L291:
L294:
li t929, 0
beq t744, t929, L276 
L277:
addi t930, $sp, 0
move $sp, t930
lw t931, 0($fp)
move $a0, t931
lw t932, 0('s0)
move $a1, t932
jal L252
addi t933, $sp, 0
move $sp, t933
addi t934, $sp, 0
move $sp, t934
la t935, L240
move $a0, t935
jal L5
addi t936, $sp, 0
move $sp, t936
addi t937, $sp, 0
move $sp, t937
lw t938, 0($fp)
move $a0, t938
lw t939, 4('s0)
move $a1, t939
jal L253
addi t940, $sp, 0
move $sp, t940
move t758, $v0
L278:
move $v0, t758
j L293 
L276:
addi t941, $sp, 0
move $sp, t941
la t942, L241
move $a0, t942
jal L5
addi t943, $sp, 0
move $sp, t943
move t758, $v0
j L278 
L293:
L296:
addi t950, $fp, -4
move t945, t950
addi t951, $sp, 0
move $sp, t951
jal L9
addi t952, $sp, 0
move $sp, t952
move t944, $v0
sw t944, 0(t945) 
addi t953, $sp, 0
move $sp, t953
move $a0, $fp
jal L250
addi t954, $sp, 0
move $sp, t954
move t759, $v0
addi t955, $fp, -4
move t947, t955
addi t956, $sp, 0
move $sp, t956
jal L9
addi t957, $sp, 0
move $sp, t957
move t946, $v0
sw t946, 0(t947) 
addi t958, $sp, 0
move $sp, t958
move $a0, $fp
jal L250
addi t959, $sp, 0
move $sp, t959
move t760, $v0
move t949, $fp
addi t960, $sp, 0
move $sp, t960
move $a0, $fp
move $a1, t759
move $a2, t760
jal L251
addi t961, $sp, 0
move $sp, t961
move t948, $v0
addi t962, $sp, 0
move $sp, t962
move $a0, t949
move $a1, t948
jal L253
addi t963, $sp, 0
move $sp, t963
move $v0, $v0
j L295 
L295:

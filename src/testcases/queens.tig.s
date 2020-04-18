L148:

L142: .
L141: O
L221:
li t494, 0
move t446, t494
lw t497, 0('s0)
lw t496, -4('s0)
li t498, 1
sub t495, t496, t498
move t452, t495
ble t446, t452, L150 
L136:
addi t499, $sp, 0
move $sp, t499
la t500, L148
move $a0, t500
jal L5
addi t501, $sp, 0
move $sp, t501
move $v0, $v0
j L220 
L149:
addi t502, t446, 1
move t446, t502
L150:
li t503, 0
move t447, t503
lw t506, 0('s0)
lw t505, -4('s0)
li t507, 1
sub t504, t505, t507
move t451, t504
ble t447, t451, L147 
L137:
addi t508, $sp, 0
move $sp, t508
la t509, L148
move $a0, t509
jal L5
addi t510, $sp, 0
move $sp, t510
blt t446, t452, L149 
L222:
j L136 
L146:
addi t511, t447, 1
move t447, t511
L147:
move t448, t446
lw t513, 0('s0)
lw t512, -12('s0)
move t449, t512
lw t514, 0(t449)
bge t448, t514, L138 
L140:
li t515, 0
blt t448, t515, L138 
L139:
addi t519, t448, 1
li t520, 4
mul t518, t519, t520
add t517, t449, t518
lw t516, 0(t517)
beq t516, t447, L143 
L144:
la t521, L142
move t450, t521
L145:
addi t522, $sp, 0
move $sp, t522
move $a0, t450
jal L5
addi t523, $sp, 0
move $sp, t523
blt t447, t451, L146 
L223:
j L137 
L138:
addi t524, $sp, 0
move $sp, t524
li t525, 1
move $a0, t525
jal exit
addi t526, $sp, 0
move $sp, t526
j L139 
L143:
la t527, L141
move t450, t527
j L145 
L220:
L225:
lw t529, 0('s0)
lw t528, -4('s0)
beq t445, t528, L217 
L218:
li t530, 0
move t453, t530
lw t533, 0('s0)
lw t532, -4('s0)
li t534, 1
sub t531, t532, t534
move t492, t531
ble t453, t492, L216 
L151:
li t535, 0
move t493, t535
L219:
move $v0, t493
j L224 
L217:
addi t536, $sp, 0
move $sp, t536
lw t537, 0($fp)
move $a0, t537
jal L134
addi t538, $sp, 0
move $sp, t538
move t493, $v0
j L219 
L215:
addi t539, t453, 1
move t453, t539
L216:
move t454, t453
lw t541, 0('s0)
lw t540, -8('s0)
move t455, t540
lw t542, 0(t455)
bge t454, t542, L152 
L154:
li t543, 0
blt t454, t543, L152 
L153:
addi t547, t454, 1
li t548, 4
mul t546, t547, t548
add t545, t455, t546
lw t544, 0(t545)
li t549, 0
beq t544, t549, L160 
L161:
li t550, 0
move t459, t550
L162:
li t551, 0
beq t459, t551, L169 
L168:
li t552, 1
move t462, t552
addi t554, t453, 7
sub t553, t554, t445
move t460, t553
lw t556, 0('s0)
lw t555, -20('s0)
move t461, t555
lw t557, 0(t461)
bge t460, t557, L163 
L165:
li t558, 0
blt t460, t558, L163 
L164:
addi t562, t460, 1
li t563, 4
mul t561, t562, t563
add t560, t461, t561
lw t559, 0(t560)
li t564, 0
beq t559, t564, L166 
L167:
li t565, 0
move t462, t565
L166:
move t463, t462
L170:
li t566, 0
beq t463, t566, L214 
L213:
move t464, t453
lw t568, 0('s0)
lw t567, -8('s0)
move t465, t567
lw t569, 0(t465)
bge t464, t569, L171 
L173:
li t570, 0
blt t464, t570, L171 
L172:
li t571, 1
addi t574, t464, 1
li t575, 4
mul t573, t574, t575
add t572, t465, t573
sw t571, 0(t572) 
add t576, t453, t445
move t466, t576
lw t578, 0('s0)
lw t577, -16('s0)
move t467, t577
lw t579, 0(t467)
bge t466, t579, L174 
L176:
li t580, 0
blt t466, t580, L174 
L175:
li t581, 1
addi t584, t466, 1
li t585, 4
mul t583, t584, t585
add t582, t467, t583
sw t581, 0(t582) 
addi t587, t453, 7
sub t586, t587, t445
move t468, t586
lw t589, 0('s0)
lw t588, -20('s0)
move t469, t588
lw t590, 0(t469)
bge t468, t590, L177 
L179:
li t591, 0
blt t468, t591, L177 
L178:
li t592, 1
addi t595, t468, 1
li t596, 4
mul t594, t595, t596
add t593, t469, t594
sw t592, 0(t593) 
move t470, t445
lw t598, 0('s0)
lw t597, -12('s0)
move t471, t597
lw t599, 0(t471)
bge t470, t599, L180 
L182:
li t600, 0
blt t470, t600, L180 
L181:
addi t603, t470, 1
li t604, 4
mul t602, t603, t604
add t601, t471, t602
sw t453, 0(t601) 
addi t605, $sp, 0
move $sp, t605
lw t606, 0($fp)
move $a0, t606
addi t607, t445, 1
move $a1, t607
jal L135
addi t608, $sp, 0
move $sp, t608
move t472, t453
lw t610, 0('s0)
lw t609, -8('s0)
move t473, t609
lw t611, 0(t473)
bge t472, t611, L183 
L185:
li t612, 0
blt t472, t612, L183 
L184:
li t613, 0
addi t616, t472, 1
li t617, 4
mul t615, t616, t617
add t614, t473, t615
sw t613, 0(t614) 
add t618, t453, t445
move t474, t618
lw t620, 0('s0)
lw t619, -16('s0)
move t475, t619
lw t621, 0(t475)
bge t474, t621, L186 
L188:
li t622, 0
blt t474, t622, L186 
L187:
li t623, 0
addi t626, t474, 1
li t627, 4
mul t625, t626, t627
add t624, t475, t625
sw t623, 0(t624) 
addi t629, t453, 7
sub t628, t629, t445
move t476, t628
lw t631, 0('s0)
lw t630, -20('s0)
move t477, t630
lw t632, 0(t477)
bge t476, t632, L189 
L191:
li t633, 0
blt t476, t633, L189 
L190:
li t634, 0
addi t637, t476, 1
li t638, 4
mul t636, t637, t638
add t635, t477, t636
sw t634, 0(t635) 
L214:
blt t453, t492, L215 
L226:
j L151 
L152:
addi t639, $sp, 0
move $sp, t639
li t640, 1
move $a0, t640
jal exit
addi t641, $sp, 0
move $sp, t641
j L153 
L160:
li t642, 1
move t458, t642
add t643, t453, t445
move t456, t643
lw t645, 0('s0)
lw t644, -16('s0)
move t457, t644
lw t646, 0(t457)
bge t456, t646, L155 
L157:
li t647, 0
blt t456, t647, L155 
L156:
addi t651, t456, 1
li t652, 4
mul t650, t651, t652
add t649, t457, t650
lw t648, 0(t649)
li t653, 0
beq t648, t653, L158 
L159:
li t654, 0
move t458, t654
L158:
move t459, t458
j L162 
L155:
addi t655, $sp, 0
move $sp, t655
li t656, 1
move $a0, t656
jal exit
addi t657, $sp, 0
move $sp, t657
j L156 
L163:
addi t658, $sp, 0
move $sp, t658
li t659, 1
move $a0, t659
jal exit
addi t660, $sp, 0
move $sp, t660
j L164 
L169:
li t661, 0
move t463, t661
j L170 
L171:
addi t662, $sp, 0
move $sp, t662
li t663, 1
move $a0, t663
jal exit
addi t664, $sp, 0
move $sp, t664
j L172 
L174:
addi t665, $sp, 0
move $sp, t665
li t666, 1
move $a0, t666
jal exit
addi t667, $sp, 0
move $sp, t667
j L175 
L177:
addi t668, $sp, 0
move $sp, t668
li t669, 1
move $a0, t669
jal exit
addi t670, $sp, 0
move $sp, t670
j L178 
L180:
addi t671, $sp, 0
move $sp, t671
li t672, 1
move $a0, t672
jal exit
addi t673, $sp, 0
move $sp, t673
j L181 
L183:
addi t674, $sp, 0
move $sp, t674
li t675, 1
move $a0, t675
jal exit
addi t676, $sp, 0
move $sp, t676
j L184 
L186:
addi t677, $sp, 0
move $sp, t677
li t678, 1
move $a0, t678
jal exit
addi t679, $sp, 0
move $sp, t679
j L187 
L189:
addi t680, $sp, 0
move $sp, t680
li t681, 1
move $a0, t681
jal exit
addi t682, $sp, 0
move $sp, t682
j L190 
L224:
L228:
li t687, 8
sw 's0, -4($fp) 
addi t688, $fp, -8
move t683, t688
addi t689, $sp, 0
move $sp, t689
lw t691, -4('s0)
addi t690, t691, 1
move $a0, t690
li t692, 0
move $a1, t692
jal initArray
addi t693, $sp, 0
move $sp, t693
move t441, $v0
lw t694, -4('s0)
sw t694, 0(t441) 
sw t441, 0(t683) 
addi t695, $fp, -12
move t684, t695
addi t696, $sp, 0
move $sp, t696
lw t698, -4('s0)
addi t697, t698, 1
move $a0, t697
li t699, 0
move $a1, t699
jal initArray
addi t700, $sp, 0
move $sp, t700
move t442, $v0
lw t701, -4('s0)
sw t701, 0(t442) 
sw t442, 0(t684) 
addi t702, $fp, -16
move t685, t702
addi t703, $sp, 0
move $sp, t703
lw t707, -4('s0)
lw t708, -4('s0)
add t706, t707, t708
li t709, 1
sub t705, t706, t709
addi t704, t705, 1
move $a0, t704
li t710, 0
move $a1, t710
jal initArray
addi t711, $sp, 0
move $sp, t711
move t443, $v0
lw t714, -4('s0)
lw t715, -4('s0)
add t713, t714, t715
li t716, 1
sub t712, t713, t716
sw t712, 0(t443) 
sw t443, 0(t685) 
addi t717, $fp, -20
move t686, t717
addi t718, $sp, 0
move $sp, t718
lw t722, -4('s0)
lw t723, -4('s0)
add t721, t722, t723
li t724, 1
sub t720, t721, t724
addi t719, t720, 1
move $a0, t719
li t725, 0
move $a1, t725
jal initArray
addi t726, $sp, 0
move $sp, t726
move t444, $v0
lw t729, -4('s0)
lw t730, -4('s0)
add t728, t729, t730
li t731, 1
sub t727, t728, t731
sw t727, 0(t444) 
sw t444, 0(t686) 
addi t732, $sp, 0
move $sp, t732
move $a0, $fp
li t733, 0
move $a1, t733
jal L135
addi t734, $sp, 0
move $sp, t734
move $v0, $v0
j L227 
L227:

import "RF00005_data.hh"

signature sig_cm(alphabet, answer) {
  answer silent_transition(float, answer; int);
  answer left_transition(float, alphabet, answer; int);
  answer right_transition(float, answer, alphabet; int);
  answer pair_transition(float, alphabet, answer, alphabet; int);
  answer bifurcation_transition(float, float, answer, answer; int);
  answer nil(float, void; int);
  choice [answer] h([answer]);
}
    

algebra alg_enum auto enum;

algebra alg_count auto count;

algebra alg_cyk implements sig_cm(alphabet=char, answer=float) {
  float silent_transition(float tsc, float x; int pos) {
    return tsc + x;
  }
  float left_transition(float tsc, alphabet a, float x; int pos) {
    return tsc + getEmission(pos, a) + x;
  }
  float right_transition(float tsc, float x, alphabet a; int pos) {
    return tsc + getEmission(pos, a) + x;
  }
  float pair_transition(float tsc, alphabet a, float x, alphabet b; int pos) {
    return tsc + getPairEmission(pos, a, b) + x;
  }
  float bifurcation_transition(float tsc_left, float tsc_right, float left, float right; int pos) {
    return tsc_left + tsc_right + left + right;
  }
  float nil(float tsc, void; int pos) {
    return tsc;
  }
  choice [float] h([float] candidates) {
    return list(maximum(candidates));
  }
}

algebra alg_inside extends alg_cyk {
  choice [float] h([float] candidates) {
    return list(bitsum(candidates));
  }
}
    
grammar gra_cm uses sig_cm(axiom = state_S_0) {
  // node [ ROOT 0 ]
  state_S_0 = silent_transition(CONST_FLOAT(-11.033000), state_IL_1; 0)
            | silent_transition(CONST_FLOAT(-12.278000), state_IR_2; 0)
            | silent_transition(CONST_FLOAT(-0.061000), state_MR_3; 0)
            | silent_transition(CONST_FLOAT(-4.629000), state_D_4; 0)
            # h;

  state_IL_1 = left_transition(CONST_FLOAT(-2.817000), CHAR, state_IL_1; 1)
             | left_transition(CONST_FLOAT(-4.319000), CHAR, state_IR_2; 1)
             | left_transition(CONST_FLOAT(-0.613000), CHAR, state_MR_3; 1)
             | left_transition(CONST_FLOAT(-2.698000), CHAR, state_D_4; 1)
             # h;

  state_IR_2 = right_transition(CONST_FLOAT(-1.925000), state_IR_2, CHAR; 2)
             | right_transition(CONST_FLOAT(-0.554000), state_MR_3, CHAR; 2)
             | right_transition(CONST_FLOAT(-4.164000), state_D_4, CHAR; 2)
             # h;

  // node [ MATR 1 ]
  state_MR_3 = right_transition(CONST_FLOAT(-11.036000), state_IR_5, CHAR; 3)
             | right_transition(CONST_FLOAT(-0.003000), state_MP_6, CHAR; 3)
             | right_transition(CONST_FLOAT(-10.852000), state_ML_7, CHAR; 3)
             | right_transition(CONST_FLOAT(-11.064000), state_MR_8, CHAR; 3)
             | right_transition(CONST_FLOAT(-11.956000), state_D_9, CHAR; 3)
             # h;

  state_D_4 = silent_transition(CONST_FLOAT(-1.727000), state_IR_5; 4)
            | silent_transition(CONST_FLOAT(-1.310000), state_MP_6; 4)
            | silent_transition(CONST_FLOAT(-4.986000), state_ML_7; 4)
            | silent_transition(CONST_FLOAT(-2.211000), state_MR_8; 4)
            | silent_transition(CONST_FLOAT(-4.411000), state_D_9; 4)
            # h;

  state_IR_5 = right_transition(CONST_FLOAT(-3.146000), state_IR_5, CHAR; 5)
             | right_transition(CONST_FLOAT(-0.277000), state_MP_6, CHAR; 5)
             | right_transition(CONST_FLOAT(-6.657000), state_ML_7, CHAR; 5)
             | right_transition(CONST_FLOAT(-4.825000), state_MR_8, CHAR; 5)
             | right_transition(CONST_FLOAT(-5.931000), state_D_9, CHAR; 5)
             # h;

  // node [ MATP 2 ]
  state_MP_6 = pair_transition(CONST_FLOAT(-12.104000), CHAR, state_IL_10, CHAR; 6)
             | pair_transition(CONST_FLOAT(-12.043000), CHAR, state_IR_11, CHAR; 6)
             | pair_transition(CONST_FLOAT(-0.003000), CHAR, state_MP_12, CHAR; 6)
             | pair_transition(CONST_FLOAT(-10.819000), CHAR, state_ML_13, CHAR; 6)
             | pair_transition(CONST_FLOAT(-11.099000), CHAR, state_MR_14, CHAR; 6)
             | pair_transition(CONST_FLOAT(-11.494000), CHAR, state_D_15, CHAR; 6)
             # h;

  state_ML_7 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_10; 7)
             | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_11; 7)
             | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_12; 7)
             | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_13; 7)
             | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_14; 7)
             | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_15; 7)
             # h;

  state_MR_8 = right_transition(CONST_FLOAT(-7.909000), state_IL_10, CHAR; 8)
             | right_transition(CONST_FLOAT(-6.638000), state_IR_11, CHAR; 8)
             | right_transition(CONST_FLOAT(-0.637000), state_MP_12, CHAR; 8)
             | right_transition(CONST_FLOAT(-6.616000), state_ML_13, CHAR; 8)
             | right_transition(CONST_FLOAT(-1.750000), state_MR_14, CHAR; 8)
             | right_transition(CONST_FLOAT(-4.829000), state_D_15, CHAR; 8)
             # h;

  state_D_9 = silent_transition(CONST_FLOAT(-9.049000), state_IL_10; 9)
            | silent_transition(CONST_FLOAT(-7.747000), state_IR_11; 9)
            | silent_transition(CONST_FLOAT(-3.544000), state_MP_12; 9)
            | silent_transition(CONST_FLOAT(-4.226000), state_ML_13; 9)
            | silent_transition(CONST_FLOAT(-4.244000), state_MR_14; 9)
            | silent_transition(CONST_FLOAT(-0.319000), state_D_15; 9)
            # h;

  state_IL_10 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_10; 10)
              | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_11; 10)
              | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_12; 10)
              | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_13; 10)
              | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_14; 10)
              | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_15; 10)
              # h;

  state_IR_11 = right_transition(CONST_FLOAT(-2.408000), state_IR_11, CHAR; 11)
              | right_transition(CONST_FLOAT(-0.496000), state_MP_12, CHAR; 11)
              | right_transition(CONST_FLOAT(-5.920000), state_ML_13, CHAR; 11)
              | right_transition(CONST_FLOAT(-4.087000), state_MR_14, CHAR; 11)
              | right_transition(CONST_FLOAT(-5.193000), state_D_15, CHAR; 11)
              # h;

  // node [ MATP 3 ]
  state_MP_12 = pair_transition(CONST_FLOAT(-12.117000), CHAR, state_IL_16, CHAR; 12)
              | pair_transition(CONST_FLOAT(-12.057000), CHAR, state_IR_17, CHAR; 12)
              | pair_transition(CONST_FLOAT(-0.003000), CHAR, state_MP_18, CHAR; 12)
              | pair_transition(CONST_FLOAT(-10.833000), CHAR, state_ML_19, CHAR; 12)
              | pair_transition(CONST_FLOAT(-11.113000), CHAR, state_MR_20, CHAR; 12)
              | pair_transition(CONST_FLOAT(-11.508000), CHAR, state_D_21, CHAR; 12)
              # h;

  state_ML_13 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_16; 13)
              | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_17; 13)
              | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_18; 13)
              | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_19; 13)
              | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_20; 13)
              | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_21; 13)
              # h;

  state_MR_14 = right_transition(CONST_FLOAT(-6.988000), state_IL_16, CHAR; 14)
              | right_transition(CONST_FLOAT(-5.717000), state_IR_17, CHAR; 14)
              | right_transition(CONST_FLOAT(-1.625000), state_MP_18, CHAR; 14)
              | right_transition(CONST_FLOAT(-5.695000), state_ML_19, CHAR; 14)
              | right_transition(CONST_FLOAT(-0.829000), state_MR_20, CHAR; 14)
              | right_transition(CONST_FLOAT(-3.908000), state_D_21, CHAR; 14)
              # h;

  state_D_15 = silent_transition(CONST_FLOAT(-9.049000), state_IL_16; 15)
             | silent_transition(CONST_FLOAT(-7.747000), state_IR_17; 15)
             | silent_transition(CONST_FLOAT(-3.544000), state_MP_18; 15)
             | silent_transition(CONST_FLOAT(-4.226000), state_ML_19; 15)
             | silent_transition(CONST_FLOAT(-4.244000), state_MR_20; 15)
             | silent_transition(CONST_FLOAT(-0.319000), state_D_21; 15)
             # h;

  state_IL_16 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_16; 16)
              | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_17; 16)
              | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_18; 16)
              | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_19; 16)
              | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_20; 16)
              | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_21; 16)
              # h;

  state_IR_17 = right_transition(CONST_FLOAT(-2.408000), state_IR_17, CHAR; 17)
              | right_transition(CONST_FLOAT(-0.496000), state_MP_18, CHAR; 17)
              | right_transition(CONST_FLOAT(-5.920000), state_ML_19, CHAR; 17)
              | right_transition(CONST_FLOAT(-4.087000), state_MR_20, CHAR; 17)
              | right_transition(CONST_FLOAT(-5.193000), state_D_21, CHAR; 17)
              # h;

  // node [ MATP 4 ]
  state_MP_18 = pair_transition(CONST_FLOAT(-12.117000), CHAR, state_IL_22, CHAR; 18)
              | pair_transition(CONST_FLOAT(-12.057000), CHAR, state_IR_23, CHAR; 18)
              | pair_transition(CONST_FLOAT(-0.003000), CHAR, state_MP_24, CHAR; 18)
              | pair_transition(CONST_FLOAT(-10.833000), CHAR, state_ML_25, CHAR; 18)
              | pair_transition(CONST_FLOAT(-11.113000), CHAR, state_MR_26, CHAR; 18)
              | pair_transition(CONST_FLOAT(-11.508000), CHAR, state_D_27, CHAR; 18)
              # h;

  state_ML_19 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_22; 19)
              | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_23; 19)
              | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_24; 19)
              | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_25; 19)
              | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_26; 19)
              | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_27; 19)
              # h;

  state_MR_20 = right_transition(CONST_FLOAT(-6.988000), state_IL_22, CHAR; 20)
              | right_transition(CONST_FLOAT(-5.717000), state_IR_23, CHAR; 20)
              | right_transition(CONST_FLOAT(-1.625000), state_MP_24, CHAR; 20)
              | right_transition(CONST_FLOAT(-5.695000), state_ML_25, CHAR; 20)
              | right_transition(CONST_FLOAT(-0.829000), state_MR_26, CHAR; 20)
              | right_transition(CONST_FLOAT(-3.908000), state_D_27, CHAR; 20)
              # h;

  state_D_21 = silent_transition(CONST_FLOAT(-9.049000), state_IL_22; 21)
             | silent_transition(CONST_FLOAT(-7.747000), state_IR_23; 21)
             | silent_transition(CONST_FLOAT(-3.544000), state_MP_24; 21)
             | silent_transition(CONST_FLOAT(-4.226000), state_ML_25; 21)
             | silent_transition(CONST_FLOAT(-4.244000), state_MR_26; 21)
             | silent_transition(CONST_FLOAT(-0.319000), state_D_27; 21)
             # h;

  state_IL_22 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_22; 22)
              | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_23; 22)
              | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_24; 22)
              | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_25; 22)
              | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_26; 22)
              | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_27; 22)
              # h;

  state_IR_23 = right_transition(CONST_FLOAT(-2.408000), state_IR_23, CHAR; 23)
              | right_transition(CONST_FLOAT(-0.496000), state_MP_24, CHAR; 23)
              | right_transition(CONST_FLOAT(-5.920000), state_ML_25, CHAR; 23)
              | right_transition(CONST_FLOAT(-4.087000), state_MR_26, CHAR; 23)
              | right_transition(CONST_FLOAT(-5.193000), state_D_27, CHAR; 23)
              # h;

  // node [ MATP 5 ]
  state_MP_24 = pair_transition(CONST_FLOAT(-12.117000), CHAR, state_IL_28, CHAR; 24)
              | pair_transition(CONST_FLOAT(-12.057000), CHAR, state_IR_29, CHAR; 24)
              | pair_transition(CONST_FLOAT(-0.003000), CHAR, state_MP_30, CHAR; 24)
              | pair_transition(CONST_FLOAT(-10.833000), CHAR, state_ML_31, CHAR; 24)
              | pair_transition(CONST_FLOAT(-11.113000), CHAR, state_MR_32, CHAR; 24)
              | pair_transition(CONST_FLOAT(-11.508000), CHAR, state_D_33, CHAR; 24)
              # h;

  state_ML_25 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_28; 25)
              | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_29; 25)
              | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_30; 25)
              | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_31; 25)
              | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_32; 25)
              | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_33; 25)
              # h;

  state_MR_26 = right_transition(CONST_FLOAT(-6.988000), state_IL_28, CHAR; 26)
              | right_transition(CONST_FLOAT(-5.717000), state_IR_29, CHAR; 26)
              | right_transition(CONST_FLOAT(-1.625000), state_MP_30, CHAR; 26)
              | right_transition(CONST_FLOAT(-5.695000), state_ML_31, CHAR; 26)
              | right_transition(CONST_FLOAT(-0.829000), state_MR_32, CHAR; 26)
              | right_transition(CONST_FLOAT(-3.908000), state_D_33, CHAR; 26)
              # h;

  state_D_27 = silent_transition(CONST_FLOAT(-9.049000), state_IL_28; 27)
             | silent_transition(CONST_FLOAT(-7.747000), state_IR_29; 27)
             | silent_transition(CONST_FLOAT(-3.544000), state_MP_30; 27)
             | silent_transition(CONST_FLOAT(-4.226000), state_ML_31; 27)
             | silent_transition(CONST_FLOAT(-4.244000), state_MR_32; 27)
             | silent_transition(CONST_FLOAT(-0.319000), state_D_33; 27)
             # h;

  state_IL_28 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_28; 28)
              | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_29; 28)
              | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_30; 28)
              | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_31; 28)
              | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_32; 28)
              | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_33; 28)
              # h;

  state_IR_29 = right_transition(CONST_FLOAT(-2.408000), state_IR_29, CHAR; 29)
              | right_transition(CONST_FLOAT(-0.496000), state_MP_30, CHAR; 29)
              | right_transition(CONST_FLOAT(-5.920000), state_ML_31, CHAR; 29)
              | right_transition(CONST_FLOAT(-4.087000), state_MR_32, CHAR; 29)
              | right_transition(CONST_FLOAT(-5.193000), state_D_33, CHAR; 29)
              # h;

  // node [ MATP 6 ]
  state_MP_30 = pair_transition(CONST_FLOAT(-12.117000), CHAR, state_IL_34, CHAR; 30)
              | pair_transition(CONST_FLOAT(-12.057000), CHAR, state_IR_35, CHAR; 30)
              | pair_transition(CONST_FLOAT(-0.003000), CHAR, state_MP_36, CHAR; 30)
              | pair_transition(CONST_FLOAT(-10.833000), CHAR, state_ML_37, CHAR; 30)
              | pair_transition(CONST_FLOAT(-11.113000), CHAR, state_MR_38, CHAR; 30)
              | pair_transition(CONST_FLOAT(-11.508000), CHAR, state_D_39, CHAR; 30)
              # h;

  state_ML_31 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_34; 31)
              | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_35; 31)
              | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_36; 31)
              | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_37; 31)
              | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_38; 31)
              | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_39; 31)
              # h;

  state_MR_32 = right_transition(CONST_FLOAT(-6.988000), state_IL_34, CHAR; 32)
              | right_transition(CONST_FLOAT(-5.717000), state_IR_35, CHAR; 32)
              | right_transition(CONST_FLOAT(-1.625000), state_MP_36, CHAR; 32)
              | right_transition(CONST_FLOAT(-5.695000), state_ML_37, CHAR; 32)
              | right_transition(CONST_FLOAT(-0.829000), state_MR_38, CHAR; 32)
              | right_transition(CONST_FLOAT(-3.908000), state_D_39, CHAR; 32)
              # h;

  state_D_33 = silent_transition(CONST_FLOAT(-9.049000), state_IL_34; 33)
             | silent_transition(CONST_FLOAT(-7.747000), state_IR_35; 33)
             | silent_transition(CONST_FLOAT(-3.544000), state_MP_36; 33)
             | silent_transition(CONST_FLOAT(-4.226000), state_ML_37; 33)
             | silent_transition(CONST_FLOAT(-4.244000), state_MR_38; 33)
             | silent_transition(CONST_FLOAT(-0.319000), state_D_39; 33)
             # h;

  state_IL_34 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_34; 34)
              | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_35; 34)
              | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_36; 34)
              | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_37; 34)
              | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_38; 34)
              | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_39; 34)
              # h;

  state_IR_35 = right_transition(CONST_FLOAT(-2.408000), state_IR_35, CHAR; 35)
              | right_transition(CONST_FLOAT(-0.496000), state_MP_36, CHAR; 35)
              | right_transition(CONST_FLOAT(-5.920000), state_ML_37, CHAR; 35)
              | right_transition(CONST_FLOAT(-4.087000), state_MR_38, CHAR; 35)
              | right_transition(CONST_FLOAT(-5.193000), state_D_39, CHAR; 35)
              # h;

  // node [ MATP 7 ]
  state_MP_36 = pair_transition(CONST_FLOAT(-12.117000), CHAR, state_IL_40, CHAR; 36)
              | pair_transition(CONST_FLOAT(-12.057000), CHAR, state_IR_41, CHAR; 36)
              | pair_transition(CONST_FLOAT(-0.004000), CHAR, state_MP_42, CHAR; 36)
              | pair_transition(CONST_FLOAT(-9.656000), CHAR, state_ML_43, CHAR; 36)
              | pair_transition(CONST_FLOAT(-11.113000), CHAR, state_MR_44, CHAR; 36)
              | pair_transition(CONST_FLOAT(-11.508000), CHAR, state_D_45, CHAR; 36)
              # h;

  state_ML_37 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_40; 37)
              | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_41; 37)
              | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_42; 37)
              | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_43; 37)
              | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_44; 37)
              | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_45; 37)
              # h;

  state_MR_38 = right_transition(CONST_FLOAT(-6.988000), state_IL_40, CHAR; 38)
              | right_transition(CONST_FLOAT(-5.717000), state_IR_41, CHAR; 38)
              | right_transition(CONST_FLOAT(-1.625000), state_MP_42, CHAR; 38)
              | right_transition(CONST_FLOAT(-5.695000), state_ML_43, CHAR; 38)
              | right_transition(CONST_FLOAT(-0.829000), state_MR_44, CHAR; 38)
              | right_transition(CONST_FLOAT(-3.908000), state_D_45, CHAR; 38)
              # h;

  state_D_39 = silent_transition(CONST_FLOAT(-9.049000), state_IL_40; 39)
             | silent_transition(CONST_FLOAT(-7.747000), state_IR_41; 39)
             | silent_transition(CONST_FLOAT(-3.544000), state_MP_42; 39)
             | silent_transition(CONST_FLOAT(-4.226000), state_ML_43; 39)
             | silent_transition(CONST_FLOAT(-4.244000), state_MR_44; 39)
             | silent_transition(CONST_FLOAT(-0.319000), state_D_45; 39)
             # h;

  state_IL_40 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_40; 40)
              | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_41; 40)
              | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_42; 40)
              | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_43; 40)
              | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_44; 40)
              | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_45; 40)
              # h;

  state_IR_41 = right_transition(CONST_FLOAT(-2.408000), state_IR_41, CHAR; 41)
              | right_transition(CONST_FLOAT(-0.496000), state_MP_42, CHAR; 41)
              | right_transition(CONST_FLOAT(-5.920000), state_ML_43, CHAR; 41)
              | right_transition(CONST_FLOAT(-4.087000), state_MR_44, CHAR; 41)
              | right_transition(CONST_FLOAT(-5.193000), state_D_45, CHAR; 41)
              # h;

  // node [ MATP 8 ]
  state_MP_42 = pair_transition(CONST_FLOAT(-11.232000), CHAR, state_IL_46, CHAR; 42)
              | pair_transition(CONST_FLOAT(-6.672000), CHAR, state_IR_47, CHAR; 42)
              | pair_transition(CONST_FLOAT(-0.016000), CHAR, state_ML_48, CHAR; 42)
              | pair_transition(CONST_FLOAT(-9.853000), CHAR, state_D_49, CHAR; 42)
              # h;

  state_ML_43 = left_transition(CONST_FLOAT(-3.836000), CHAR, state_IL_46; 43)
              | left_transition(CONST_FLOAT(-4.018000), CHAR, state_IR_47; 43)
              | left_transition(CONST_FLOAT(-0.475000), CHAR, state_ML_48; 43)
              | left_transition(CONST_FLOAT(-2.747000), CHAR, state_D_49; 43)
              # h;

  state_MR_44 = right_transition(CONST_FLOAT(-4.809000), state_IL_46, CHAR; 44)
              | right_transition(CONST_FLOAT(-3.838000), state_IR_47, CHAR; 44)
              | right_transition(CONST_FLOAT(-1.706000), state_ML_48, CHAR; 44)
              | right_transition(CONST_FLOAT(-0.766000), state_D_49, CHAR; 44)
              # h;

  state_D_45 = silent_transition(CONST_FLOAT(-4.568000), state_IL_46; 45)
             | silent_transition(CONST_FLOAT(-4.250000), state_IR_47; 45)
             | silent_transition(CONST_FLOAT(-2.265000), state_ML_48; 45)
             | silent_transition(CONST_FLOAT(-0.520000), state_D_49; 45)
             # h;

  state_IL_46 = left_transition(CONST_FLOAT(-1.686000), CHAR, state_IL_46; 46)
              | left_transition(CONST_FLOAT(-2.369000), CHAR, state_IR_47; 46)
              | left_transition(CONST_FLOAT(-1.117000), CHAR, state_ML_48; 46)
              | left_transition(CONST_FLOAT(-4.855000), CHAR, state_D_49; 46)
              # h;

  state_IR_47 = right_transition(CONST_FLOAT(-1.924000), state_IR_47, CHAR; 47)
              | right_transition(CONST_FLOAT(-0.523000), state_ML_48, CHAR; 47)
              | right_transition(CONST_FLOAT(-4.624000), state_D_49, CHAR; 47)
              # h;

  // node [ MATL 9 ]
  state_ML_48 = left_transition(CONST_FLOAT(-5.352000), CHAR, state_IL_50; 48)
              | left_transition(CONST_FLOAT(-0.048000), CHAR, state_ML_51; 48)
              | left_transition(CONST_FLOAT(-6.940000), CHAR, state_D_52; 48)
              # h;

  state_D_49 = silent_transition(CONST_FLOAT(-6.174000), state_IL_50; 49)
             | silent_transition(CONST_FLOAT(-1.687000), state_ML_51; 49)
             | silent_transition(CONST_FLOAT(-0.566000), state_D_52; 49)
             # h;

  state_IL_50 = left_transition(CONST_FLOAT(-2.459000), CHAR, state_IL_50; 50)
              | left_transition(CONST_FLOAT(-0.340000), CHAR, state_ML_51; 50)
              | left_transition(CONST_FLOAT(-5.159000), CHAR, state_D_52; 50)
              # h;

  // node [ MATL 10 ]
  state_ML_51 = left_transition(CONST_FLOAT(-5.143000), CHAR, state_IL_53; 51)
              | left_transition(CONST_FLOAT(-0.041000), CHAR, state_B_54; 51)
              # h;

  state_D_52 = silent_transition(CONST_FLOAT(-8.552000), state_IL_53; 52)
             | silent_transition(CONST_FLOAT(-0.004000), state_B_54; 52)
             # h;

  state_IL_53 = left_transition(CONST_FLOAT(-3.425000), CHAR, state_IL_53; 53)
              | left_transition(CONST_FLOAT(-0.141000), CHAR, state_B_54; 53)
              # h;

  // node [ BIF 11 ]
  state_B_54 = bifurcation_transition(CONST_FLOAT(0.000000), CONST_FLOAT(0.000000), state_S_121, state_S_55; 54)
             # h;

  // node [ BEGR 42 ]
  state_S_55 = silent_transition(CONST_FLOAT(-12.148000), state_IL_56; 55)
             | silent_transition(CONST_FLOAT(-0.001000), state_ML_57; 55)
             | silent_transition(CONST_FLOAT(-10.802000), state_D_58; 55)
             # h;

  state_IL_56 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_56; 56)
              | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_57; 56)
              | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_58; 56)
              # h;

  // node [ MATL 43 ]
  state_ML_57 = left_transition(CONST_FLOAT(-12.148000), CHAR, state_IL_59; 57)
              | left_transition(CONST_FLOAT(-0.425000), CHAR, state_ML_60; 57)
              | left_transition(CONST_FLOAT(-1.972000), CHAR, state_D_61; 57)
              # h;

  state_D_58 = silent_transition(CONST_FLOAT(-6.174000), state_IL_59; 58)
             | silent_transition(CONST_FLOAT(-1.687000), state_ML_60; 58)
             | silent_transition(CONST_FLOAT(-0.566000), state_D_61; 58)
             # h;

  state_IL_59 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_59; 59)
              | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_60; 59)
              | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_61; 59)
              # h;

  // node [ MATL 44 ]
  state_ML_60 = left_transition(CONST_FLOAT(-4.403000), CHAR, state_IL_62; 60)
              | left_transition(CONST_FLOAT(-0.079000), CHAR, state_ML_63; 60)
              | left_transition(CONST_FLOAT(-7.327000), CHAR, state_D_64; 60)
              # h;

  state_D_61 = silent_transition(CONST_FLOAT(-0.051000), state_IL_62; 61)
             | silent_transition(CONST_FLOAT(-5.676000), state_ML_63; 61)
             | silent_transition(CONST_FLOAT(-6.016000), state_D_64; 61)
             # h;

  state_IL_62 = left_transition(CONST_FLOAT(-0.113000), CHAR, state_IL_62; 62)
              | left_transition(CONST_FLOAT(-4.469000), CHAR, state_ML_63; 62)
              | left_transition(CONST_FLOAT(-5.038000), CHAR, state_D_64; 62)
              # h;

  // node [ MATL 45 ]
  state_ML_63 = left_transition(CONST_FLOAT(-1.234000), CHAR, state_IL_65; 63)
              | left_transition(CONST_FLOAT(-0.803000), CHAR, state_ML_66; 63)
              | left_transition(CONST_FLOAT(-9.120000), CHAR, state_D_67; 63)
              # h;

  state_D_64 = silent_transition(CONST_FLOAT(-6.563000), state_IL_65; 64)
             | silent_transition(CONST_FLOAT(-0.061000), state_ML_66; 64)
             | silent_transition(CONST_FLOAT(-5.013000), state_D_67; 64)
             # h;

  state_IL_65 = left_transition(CONST_FLOAT(-3.838000), CHAR, state_IL_65; 65)
              | left_transition(CONST_FLOAT(-0.110000), CHAR, state_ML_66; 65)
              | left_transition(CONST_FLOAT(-8.281000), CHAR, state_D_67; 65)
              # h;

  // node [ MATL 46 ]
  state_ML_66 = left_transition(CONST_FLOAT(-11.090000), CHAR, state_IL_68; 66)
              | left_transition(CONST_FLOAT(-0.002000), CHAR, state_MP_69; 66)
              | left_transition(CONST_FLOAT(-10.906000), CHAR, state_ML_70; 66)
              | left_transition(CONST_FLOAT(-11.118000), CHAR, state_MR_71; 66)
              | left_transition(CONST_FLOAT(-12.010000), CHAR, state_D_72; 66)
              # h;

  state_D_67 = silent_transition(CONST_FLOAT(-5.092000), state_IL_68; 67)
             | silent_transition(CONST_FLOAT(-0.712000), state_MP_69; 67)
             | silent_transition(CONST_FLOAT(-4.353000), state_ML_70; 67)
             | silent_transition(CONST_FLOAT(-2.728000), state_MR_71; 67)
             | silent_transition(CONST_FLOAT(-2.640000), state_D_72; 67)
             # h;

  state_IL_68 = left_transition(CONST_FLOAT(-2.408000), CHAR, state_IL_68; 68)
              | left_transition(CONST_FLOAT(-0.496000), CHAR, state_MP_69; 68)
              | left_transition(CONST_FLOAT(-4.087000), CHAR, state_ML_70; 68)
              | left_transition(CONST_FLOAT(-5.920000), CHAR, state_MR_71; 68)
              | left_transition(CONST_FLOAT(-5.193000), CHAR, state_D_72; 68)
              # h;

  // node [ MATP 47 ]
  state_MP_69 = pair_transition(CONST_FLOAT(-12.117000), CHAR, state_IL_73, CHAR; 69)
              | pair_transition(CONST_FLOAT(-6.010000), CHAR, state_IR_74, CHAR; 69)
              | pair_transition(CONST_FLOAT(-0.025000), CHAR, state_MP_75, CHAR; 69)
              | pair_transition(CONST_FLOAT(-10.833000), CHAR, state_ML_76, CHAR; 69)
              | pair_transition(CONST_FLOAT(-11.113000), CHAR, state_MR_77, CHAR; 69)
              | pair_transition(CONST_FLOAT(-11.508000), CHAR, state_D_78, CHAR; 69)
              # h;

  state_ML_70 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_73; 70)
              | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_74; 70)
              | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_75; 70)
              | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_76; 70)
              | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_77; 70)
              | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_78; 70)
              # h;

  state_MR_71 = right_transition(CONST_FLOAT(-6.988000), state_IL_73, CHAR; 71)
              | right_transition(CONST_FLOAT(-5.717000), state_IR_74, CHAR; 71)
              | right_transition(CONST_FLOAT(-1.625000), state_MP_75, CHAR; 71)
              | right_transition(CONST_FLOAT(-5.695000), state_ML_76, CHAR; 71)
              | right_transition(CONST_FLOAT(-0.829000), state_MR_77, CHAR; 71)
              | right_transition(CONST_FLOAT(-3.908000), state_D_78, CHAR; 71)
              # h;

  state_D_72 = silent_transition(CONST_FLOAT(-9.049000), state_IL_73; 72)
             | silent_transition(CONST_FLOAT(-7.747000), state_IR_74; 72)
             | silent_transition(CONST_FLOAT(-3.544000), state_MP_75; 72)
             | silent_transition(CONST_FLOAT(-4.226000), state_ML_76; 72)
             | silent_transition(CONST_FLOAT(-4.244000), state_MR_77; 72)
             | silent_transition(CONST_FLOAT(-0.319000), state_D_78; 72)
             # h;

  state_IL_73 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_73; 73)
              | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_74; 73)
              | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_75; 73)
              | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_76; 73)
              | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_77; 73)
              | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_78; 73)
              # h;

  state_IR_74 = right_transition(CONST_FLOAT(-3.202000), state_IR_74, CHAR; 74)
              | right_transition(CONST_FLOAT(-0.265000), state_MP_75, CHAR; 74)
              | right_transition(CONST_FLOAT(-6.713000), state_ML_76, CHAR; 74)
              | right_transition(CONST_FLOAT(-4.881000), state_MR_77, CHAR; 74)
              | right_transition(CONST_FLOAT(-5.987000), state_D_78, CHAR; 74)
              # h;

  // node [ MATP 48 ]
  state_MP_75 = pair_transition(CONST_FLOAT(-12.117000), CHAR, state_IL_79, CHAR; 75)
              | pair_transition(CONST_FLOAT(-12.057000), CHAR, state_IR_80, CHAR; 75)
              | pair_transition(CONST_FLOAT(-0.003000), CHAR, state_MP_81, CHAR; 75)
              | pair_transition(CONST_FLOAT(-10.833000), CHAR, state_ML_82, CHAR; 75)
              | pair_transition(CONST_FLOAT(-11.113000), CHAR, state_MR_83, CHAR; 75)
              | pair_transition(CONST_FLOAT(-11.508000), CHAR, state_D_84, CHAR; 75)
              # h;

  state_ML_76 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_79; 76)
              | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_80; 76)
              | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_81; 76)
              | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_82; 76)
              | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_83; 76)
              | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_84; 76)
              # h;

  state_MR_77 = right_transition(CONST_FLOAT(-6.988000), state_IL_79, CHAR; 77)
              | right_transition(CONST_FLOAT(-5.717000), state_IR_80, CHAR; 77)
              | right_transition(CONST_FLOAT(-1.625000), state_MP_81, CHAR; 77)
              | right_transition(CONST_FLOAT(-5.695000), state_ML_82, CHAR; 77)
              | right_transition(CONST_FLOAT(-0.829000), state_MR_83, CHAR; 77)
              | right_transition(CONST_FLOAT(-3.908000), state_D_84, CHAR; 77)
              # h;

  state_D_78 = silent_transition(CONST_FLOAT(-9.049000), state_IL_79; 78)
             | silent_transition(CONST_FLOAT(-7.747000), state_IR_80; 78)
             | silent_transition(CONST_FLOAT(-3.544000), state_MP_81; 78)
             | silent_transition(CONST_FLOAT(-4.226000), state_ML_82; 78)
             | silent_transition(CONST_FLOAT(-4.244000), state_MR_83; 78)
             | silent_transition(CONST_FLOAT(-0.319000), state_D_84; 78)
             # h;

  state_IL_79 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_79; 79)
              | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_80; 79)
              | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_81; 79)
              | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_82; 79)
              | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_83; 79)
              | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_84; 79)
              # h;

  state_IR_80 = right_transition(CONST_FLOAT(-2.408000), state_IR_80, CHAR; 80)
              | right_transition(CONST_FLOAT(-0.496000), state_MP_81, CHAR; 80)
              | right_transition(CONST_FLOAT(-5.920000), state_ML_82, CHAR; 80)
              | right_transition(CONST_FLOAT(-4.087000), state_MR_83, CHAR; 80)
              | right_transition(CONST_FLOAT(-5.193000), state_D_84, CHAR; 80)
              # h;

  // node [ MATP 49 ]
  state_MP_81 = pair_transition(CONST_FLOAT(-6.146000), CHAR, state_IL_85, CHAR; 81)
              | pair_transition(CONST_FLOAT(-12.057000), CHAR, state_IR_86, CHAR; 81)
              | pair_transition(CONST_FLOAT(-0.026000), CHAR, state_MP_87, CHAR; 81)
              | pair_transition(CONST_FLOAT(-10.833000), CHAR, state_ML_88, CHAR; 81)
              | pair_transition(CONST_FLOAT(-11.113000), CHAR, state_MR_89, CHAR; 81)
              | pair_transition(CONST_FLOAT(-8.649000), CHAR, state_D_90, CHAR; 81)
              # h;

  state_ML_82 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_85; 82)
              | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_86; 82)
              | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_87; 82)
              | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_88; 82)
              | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_89; 82)
              | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_90; 82)
              # h;

  state_MR_83 = right_transition(CONST_FLOAT(-6.988000), state_IL_85, CHAR; 83)
              | right_transition(CONST_FLOAT(-5.717000), state_IR_86, CHAR; 83)
              | right_transition(CONST_FLOAT(-1.625000), state_MP_87, CHAR; 83)
              | right_transition(CONST_FLOAT(-5.695000), state_ML_88, CHAR; 83)
              | right_transition(CONST_FLOAT(-0.829000), state_MR_89, CHAR; 83)
              | right_transition(CONST_FLOAT(-3.908000), state_D_90, CHAR; 83)
              # h;

  state_D_84 = silent_transition(CONST_FLOAT(-9.049000), state_IL_85; 84)
             | silent_transition(CONST_FLOAT(-7.747000), state_IR_86; 84)
             | silent_transition(CONST_FLOAT(-3.544000), state_MP_87; 84)
             | silent_transition(CONST_FLOAT(-4.226000), state_ML_88; 84)
             | silent_transition(CONST_FLOAT(-4.244000), state_MR_89; 84)
             | silent_transition(CONST_FLOAT(-0.319000), state_D_90; 84)
             # h;

  state_IL_85 = left_transition(CONST_FLOAT(-3.649000), CHAR, state_IL_85; 85)
              | left_transition(CONST_FLOAT(-3.912000), CHAR, state_IR_86; 85)
              | left_transition(CONST_FLOAT(-0.313000), CHAR, state_MP_87; 85)
              | left_transition(CONST_FLOAT(-5.567000), CHAR, state_ML_88; 85)
              | left_transition(CONST_FLOAT(-6.343000), CHAR, state_MR_89; 85)
              | left_transition(CONST_FLOAT(-6.004000), CHAR, state_D_90; 85)
              # h;

  state_IR_86 = right_transition(CONST_FLOAT(-2.408000), state_IR_86, CHAR; 86)
              | right_transition(CONST_FLOAT(-0.496000), state_MP_87, CHAR; 86)
              | right_transition(CONST_FLOAT(-5.920000), state_ML_88, CHAR; 86)
              | right_transition(CONST_FLOAT(-4.087000), state_MR_89, CHAR; 86)
              | right_transition(CONST_FLOAT(-5.193000), state_D_90, CHAR; 86)
              # h;

  // node [ MATP 50 ]
  state_MP_87 = pair_transition(CONST_FLOAT(-12.114000), CHAR, state_IL_91, CHAR; 87)
              | pair_transition(CONST_FLOAT(-12.054000), CHAR, state_IR_92, CHAR; 87)
              | pair_transition(CONST_FLOAT(-0.078000), CHAR, state_MP_93, CHAR; 87)
              | pair_transition(CONST_FLOAT(-8.063000), CHAR, state_ML_94, CHAR; 87)
              | pair_transition(CONST_FLOAT(-11.110000), CHAR, state_MR_95, CHAR; 87)
              | pair_transition(CONST_FLOAT(-4.379000), CHAR, state_D_96, CHAR; 87)
              # h;

  state_ML_88 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_91; 88)
              | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_92; 88)
              | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_93; 88)
              | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_94; 88)
              | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_95; 88)
              | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_96; 88)
              # h;

  state_MR_89 = right_transition(CONST_FLOAT(-6.988000), state_IL_91, CHAR; 89)
              | right_transition(CONST_FLOAT(-5.717000), state_IR_92, CHAR; 89)
              | right_transition(CONST_FLOAT(-1.625000), state_MP_93, CHAR; 89)
              | right_transition(CONST_FLOAT(-5.695000), state_ML_94, CHAR; 89)
              | right_transition(CONST_FLOAT(-0.829000), state_MR_95, CHAR; 89)
              | right_transition(CONST_FLOAT(-3.908000), state_D_96, CHAR; 89)
              # h;

  state_D_90 = silent_transition(CONST_FLOAT(-9.420000), state_IL_91; 90)
             | silent_transition(CONST_FLOAT(-8.118000), state_IR_92; 90)
             | silent_transition(CONST_FLOAT(-3.915000), state_MP_93; 90)
             | silent_transition(CONST_FLOAT(-4.597000), state_ML_94; 90)
             | silent_transition(CONST_FLOAT(-4.615000), state_MR_95; 90)
             | silent_transition(CONST_FLOAT(-0.240000), state_D_96; 90)
             # h;

  state_IL_91 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_91; 91)
              | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_92; 91)
              | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_93; 91)
              | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_94; 91)
              | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_95; 91)
              | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_96; 91)
              # h;

  state_IR_92 = right_transition(CONST_FLOAT(-2.408000), state_IR_92, CHAR; 92)
              | right_transition(CONST_FLOAT(-0.496000), state_MP_93, CHAR; 92)
              | right_transition(CONST_FLOAT(-5.920000), state_ML_94, CHAR; 92)
              | right_transition(CONST_FLOAT(-4.087000), state_MR_95, CHAR; 92)
              | right_transition(CONST_FLOAT(-5.193000), state_D_96, CHAR; 92)
              # h;

  // node [ MATP 51 ]
  state_MP_93 = pair_transition(CONST_FLOAT(-11.148000), CHAR, state_IL_97, CHAR; 93)
              | pair_transition(CONST_FLOAT(-11.355000), CHAR, state_IR_98, CHAR; 93)
              | pair_transition(CONST_FLOAT(-0.030000), CHAR, state_ML_99, CHAR; 93)
              | pair_transition(CONST_FLOAT(-5.670000), CHAR, state_D_100, CHAR; 93)
              # h;

  state_ML_94 = left_transition(CONST_FLOAT(-4.084000), CHAR, state_IL_97; 94)
              | left_transition(CONST_FLOAT(-4.267000), CHAR, state_IR_98; 94)
              | left_transition(CONST_FLOAT(-0.389000), CHAR, state_ML_99; 94)
              | left_transition(CONST_FLOAT(-2.996000), CHAR, state_D_100; 94)
              # h;

  state_MR_95 = right_transition(CONST_FLOAT(-4.809000), state_IL_97, CHAR; 95)
              | right_transition(CONST_FLOAT(-3.838000), state_IR_98, CHAR; 95)
              | right_transition(CONST_FLOAT(-1.706000), state_ML_99, CHAR; 95)
              | right_transition(CONST_FLOAT(-0.766000), state_D_100, CHAR; 95)
              # h;

  state_D_96 = silent_transition(CONST_FLOAT(-7.445000), state_IL_97; 96)
             | silent_transition(CONST_FLOAT(-7.126000), state_IR_98; 96)
             | silent_transition(CONST_FLOAT(-0.812000), state_ML_99; 96)
             | silent_transition(CONST_FLOAT(-1.260000), state_D_100; 96)
             # h;

  state_IL_97 = left_transition(CONST_FLOAT(-1.686000), CHAR, state_IL_97; 97)
              | left_transition(CONST_FLOAT(-2.369000), CHAR, state_IR_98; 97)
              | left_transition(CONST_FLOAT(-1.117000), CHAR, state_ML_99; 97)
              | left_transition(CONST_FLOAT(-4.855000), CHAR, state_D_100; 97)
              # h;

  state_IR_98 = right_transition(CONST_FLOAT(-1.442000), state_IR_98, CHAR; 98)
              | right_transition(CONST_FLOAT(-0.798000), state_ML_99, CHAR; 98)
              | right_transition(CONST_FLOAT(-4.142000), state_D_100, CHAR; 98)
              # h;

  // node [ MATL 52 ]
  state_ML_99 = left_transition(CONST_FLOAT(-6.272000), CHAR, state_IL_101; 99)
              | left_transition(CONST_FLOAT(-0.020000), CHAR, state_ML_102; 99)
              | left_transition(CONST_FLOAT(-10.747000), CHAR, state_D_103; 99)
              # h;

  state_D_100 = silent_transition(CONST_FLOAT(-9.039000), state_IL_101; 100)
              | silent_transition(CONST_FLOAT(-0.168000), state_ML_102; 100)
              | silent_transition(CONST_FLOAT(-3.210000), state_D_103; 100)
              # h;

  state_IL_101 = left_transition(CONST_FLOAT(-2.042000), CHAR, state_IL_101; 101)
               | left_transition(CONST_FLOAT(-0.474000), CHAR, state_ML_102; 101)
               | left_transition(CONST_FLOAT(-4.742000), CHAR, state_D_103; 101)
               # h;

  // node [ MATL 53 ]
  state_ML_102 = left_transition(CONST_FLOAT(-12.147000), CHAR, state_IL_104; 102)
               | left_transition(CONST_FLOAT(-0.002000), CHAR, state_ML_105; 102)
               | left_transition(CONST_FLOAT(-9.386000), CHAR, state_D_106; 102)
               # h;

  state_D_103 = silent_transition(CONST_FLOAT(-6.327000), state_IL_104; 103)
              | silent_transition(CONST_FLOAT(-1.396000), state_ML_105; 103)
              | silent_transition(CONST_FLOAT(-0.719000), state_D_106; 103)
              # h;

  state_IL_104 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_104; 104)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_105; 104)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_106; 104)
               # h;

  // node [ MATL 54 ]
  state_ML_105 = left_transition(CONST_FLOAT(-12.146000), CHAR, state_IL_107; 105)
               | left_transition(CONST_FLOAT(-0.002000), CHAR, state_ML_108; 105)
               | left_transition(CONST_FLOAT(-9.678000), CHAR, state_D_109; 105)
               # h;

  state_D_106 = silent_transition(CONST_FLOAT(-6.384000), state_IL_107; 106)
              | silent_transition(CONST_FLOAT(-1.897000), state_ML_108; 106)
              | silent_transition(CONST_FLOAT(-0.475000), state_D_109; 106)
              # h;

  state_IL_107 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_107; 107)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_108; 107)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_109; 107)
               # h;

  // node [ MATL 55 ]
  state_ML_108 = left_transition(CONST_FLOAT(-5.589000), CHAR, state_IL_110; 108)
               | left_transition(CONST_FLOAT(-0.037000), CHAR, state_ML_111; 108)
               | left_transition(CONST_FLOAT(-7.884000), CHAR, state_D_112; 108)
               # h;

  state_D_109 = silent_transition(CONST_FLOAT(-6.516000), state_IL_110; 109)
              | silent_transition(CONST_FLOAT(-1.133000), state_ML_111; 109)
              | silent_transition(CONST_FLOAT(-0.908000), state_D_112; 109)
              # h;

  state_IL_110 = left_transition(CONST_FLOAT(-2.342000), CHAR, state_IL_110; 110)
               | left_transition(CONST_FLOAT(-0.373000), CHAR, state_ML_111; 110)
               | left_transition(CONST_FLOAT(-5.042000), CHAR, state_D_112; 110)
               # h;

  // node [ MATL 56 ]
  state_ML_111 = left_transition(CONST_FLOAT(-12.142000), CHAR, state_IL_113; 111)
               | left_transition(CONST_FLOAT(-0.043000), CHAR, state_ML_114; 111)
               | left_transition(CONST_FLOAT(-5.107000), CHAR, state_D_115; 111)
               # h;

  state_D_112 = silent_transition(CONST_FLOAT(-6.866000), state_IL_113; 112)
              | silent_transition(CONST_FLOAT(-2.379000), state_ML_114; 112)
              | silent_transition(CONST_FLOAT(-0.323000), state_D_115; 112)
              # h;

  state_IL_113 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_113; 113)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_114; 113)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_115; 113)
               # h;

  // node [ MATL 57 ]
  state_ML_114 = left_transition(CONST_FLOAT(-4.739000), CHAR, state_IL_116; 114)
               | left_transition(CONST_FLOAT(-0.155000), CHAR, state_ML_117; 114)
               | left_transition(CONST_FLOAT(-3.959000), CHAR, state_D_118; 114)
               # h;

  state_D_115 = silent_transition(CONST_FLOAT(-3.742000), state_IL_116; 115)
              | silent_transition(CONST_FLOAT(-3.655000), state_ML_117; 115)
              | silent_transition(CONST_FLOAT(-0.241000), state_D_118; 115)
              # h;

  state_IL_116 = left_transition(CONST_FLOAT(-1.931000), CHAR, state_IL_116; 116)
               | left_transition(CONST_FLOAT(-0.475000), CHAR, state_ML_117; 116)
               | left_transition(CONST_FLOAT(-5.762000), CHAR, state_D_118; 116)
               # h;

  // node [ MATL 58 ]
  state_ML_117 = left_transition(CONST_FLOAT(0.000000), CHAR, state_E_120; 117)
               # h;

  state_D_118 = silent_transition(CONST_FLOAT(0.000000), state_E_120; 118)
              # h;

  state_IL_119 = left_transition(CONST_FLOAT(-1.823000), CHAR, state_IL_119; 119)
               | left_transition(CONST_FLOAT(-0.479000), CHAR, state_E_120; 119)
               # h;

  // node [ END 59 ]
  state_E_120 = nil(CONST_FLOAT(0.000000), EMPTY; 120)
              # h;

  // node [ BEGL 12 ]
  state_S_121 = silent_transition(CONST_FLOAT(0.000000), state_B_122; 121)
              # h;

  // node [ BIF 13 ]
  state_B_122 = bifurcation_transition(CONST_FLOAT(0.000000), CONST_FLOAT(0.000000), state_S_123, state_S_170; 122)
              # h;

  // node [ BEGL 14 ]
  state_S_123 = silent_transition(CONST_FLOAT(-0.004000), state_MP_124; 123)
              | silent_transition(CONST_FLOAT(-10.203000), state_ML_125; 123)
              | silent_transition(CONST_FLOAT(-9.611000), state_MR_126; 123)
              | silent_transition(CONST_FLOAT(-10.251000), state_D_127; 123)
              # h;

  // node [ MATP 15 ]
  state_MP_124 = pair_transition(CONST_FLOAT(-12.117000), CHAR, state_IL_128, CHAR; 124)
               | pair_transition(CONST_FLOAT(-12.057000), CHAR, state_IR_129, CHAR; 124)
               | pair_transition(CONST_FLOAT(-0.003000), CHAR, state_MP_130, CHAR; 124)
               | pair_transition(CONST_FLOAT(-10.833000), CHAR, state_ML_131, CHAR; 124)
               | pair_transition(CONST_FLOAT(-11.113000), CHAR, state_MR_132, CHAR; 124)
               | pair_transition(CONST_FLOAT(-11.508000), CHAR, state_D_133, CHAR; 124)
               # h;

  state_ML_125 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_128; 125)
               | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_129; 125)
               | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_130; 125)
               | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_131; 125)
               | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_132; 125)
               | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_133; 125)
               # h;

  state_MR_126 = right_transition(CONST_FLOAT(-6.988000), state_IL_128, CHAR; 126)
               | right_transition(CONST_FLOAT(-5.717000), state_IR_129, CHAR; 126)
               | right_transition(CONST_FLOAT(-1.625000), state_MP_130, CHAR; 126)
               | right_transition(CONST_FLOAT(-5.695000), state_ML_131, CHAR; 126)
               | right_transition(CONST_FLOAT(-0.829000), state_MR_132, CHAR; 126)
               | right_transition(CONST_FLOAT(-3.908000), state_D_133, CHAR; 126)
               # h;

  state_D_127 = silent_transition(CONST_FLOAT(-9.049000), state_IL_128; 127)
              | silent_transition(CONST_FLOAT(-7.747000), state_IR_129; 127)
              | silent_transition(CONST_FLOAT(-3.544000), state_MP_130; 127)
              | silent_transition(CONST_FLOAT(-4.226000), state_ML_131; 127)
              | silent_transition(CONST_FLOAT(-4.244000), state_MR_132; 127)
              | silent_transition(CONST_FLOAT(-0.319000), state_D_133; 127)
              # h;

  state_IL_128 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_128; 128)
               | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_129; 128)
               | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_130; 128)
               | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_131; 128)
               | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_132; 128)
               | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_133; 128)
               # h;

  state_IR_129 = right_transition(CONST_FLOAT(-2.408000), state_IR_129, CHAR; 129)
               | right_transition(CONST_FLOAT(-0.496000), state_MP_130, CHAR; 129)
               | right_transition(CONST_FLOAT(-5.920000), state_ML_131, CHAR; 129)
               | right_transition(CONST_FLOAT(-4.087000), state_MR_132, CHAR; 129)
               | right_transition(CONST_FLOAT(-5.193000), state_D_133, CHAR; 129)
               # h;

  // node [ MATP 16 ]
  state_MP_130 = pair_transition(CONST_FLOAT(-12.117000), CHAR, state_IL_134, CHAR; 130)
               | pair_transition(CONST_FLOAT(-12.057000), CHAR, state_IR_135, CHAR; 130)
               | pair_transition(CONST_FLOAT(-0.004000), CHAR, state_MP_136, CHAR; 130)
               | pair_transition(CONST_FLOAT(-9.619000), CHAR, state_ML_137, CHAR; 130)
               | pair_transition(CONST_FLOAT(-11.113000), CHAR, state_MR_138, CHAR; 130)
               | pair_transition(CONST_FLOAT(-11.508000), CHAR, state_D_139, CHAR; 130)
               # h;

  state_ML_131 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_134; 131)
               | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_135; 131)
               | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_136; 131)
               | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_137; 131)
               | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_138; 131)
               | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_139; 131)
               # h;

  state_MR_132 = right_transition(CONST_FLOAT(-6.988000), state_IL_134, CHAR; 132)
               | right_transition(CONST_FLOAT(-5.717000), state_IR_135, CHAR; 132)
               | right_transition(CONST_FLOAT(-1.625000), state_MP_136, CHAR; 132)
               | right_transition(CONST_FLOAT(-5.695000), state_ML_137, CHAR; 132)
               | right_transition(CONST_FLOAT(-0.829000), state_MR_138, CHAR; 132)
               | right_transition(CONST_FLOAT(-3.908000), state_D_139, CHAR; 132)
               # h;

  state_D_133 = silent_transition(CONST_FLOAT(-9.049000), state_IL_134; 133)
              | silent_transition(CONST_FLOAT(-7.747000), state_IR_135; 133)
              | silent_transition(CONST_FLOAT(-3.544000), state_MP_136; 133)
              | silent_transition(CONST_FLOAT(-4.226000), state_ML_137; 133)
              | silent_transition(CONST_FLOAT(-4.244000), state_MR_138; 133)
              | silent_transition(CONST_FLOAT(-0.319000), state_D_139; 133)
              # h;

  state_IL_134 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_134; 134)
               | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_135; 134)
               | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_136; 134)
               | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_137; 134)
               | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_138; 134)
               | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_139; 134)
               # h;

  state_IR_135 = right_transition(CONST_FLOAT(-2.408000), state_IR_135, CHAR; 135)
               | right_transition(CONST_FLOAT(-0.496000), state_MP_136, CHAR; 135)
               | right_transition(CONST_FLOAT(-5.920000), state_ML_137, CHAR; 135)
               | right_transition(CONST_FLOAT(-4.087000), state_MR_138, CHAR; 135)
               | right_transition(CONST_FLOAT(-5.193000), state_D_139, CHAR; 135)
               # h;

  // node [ MATP 17 ]
  state_MP_136 = pair_transition(CONST_FLOAT(-12.116000), CHAR, state_IL_140, CHAR; 136)
               | pair_transition(CONST_FLOAT(-12.056000), CHAR, state_IR_141, CHAR; 136)
               | pair_transition(CONST_FLOAT(-0.003000), CHAR, state_MP_142, CHAR; 136)
               | pair_transition(CONST_FLOAT(-10.832000), CHAR, state_ML_143, CHAR; 136)
               | pair_transition(CONST_FLOAT(-11.112000), CHAR, state_MR_144, CHAR; 136)
               | pair_transition(CONST_FLOAT(-11.507000), CHAR, state_D_145, CHAR; 136)
               # h;

  state_ML_137 = left_transition(CONST_FLOAT(-6.358000), CHAR, state_IL_140; 137)
               | left_transition(CONST_FLOAT(-6.704000), CHAR, state_IR_141; 137)
               | left_transition(CONST_FLOAT(-1.164000), CHAR, state_MP_142; 137)
               | left_transition(CONST_FLOAT(-1.113000), CHAR, state_ML_143; 137)
               | left_transition(CONST_FLOAT(-6.554000), CHAR, state_MR_144; 137)
               | left_transition(CONST_FLOAT(-4.083000), CHAR, state_D_145; 137)
               # h;

  state_MR_138 = right_transition(CONST_FLOAT(-6.988000), state_IL_140, CHAR; 138)
               | right_transition(CONST_FLOAT(-5.717000), state_IR_141, CHAR; 138)
               | right_transition(CONST_FLOAT(-1.625000), state_MP_142, CHAR; 138)
               | right_transition(CONST_FLOAT(-5.695000), state_ML_143, CHAR; 138)
               | right_transition(CONST_FLOAT(-0.829000), state_MR_144, CHAR; 138)
               | right_transition(CONST_FLOAT(-3.908000), state_D_145, CHAR; 138)
               # h;

  state_D_139 = silent_transition(CONST_FLOAT(-9.049000), state_IL_140; 139)
              | silent_transition(CONST_FLOAT(-7.747000), state_IR_141; 139)
              | silent_transition(CONST_FLOAT(-3.544000), state_MP_142; 139)
              | silent_transition(CONST_FLOAT(-4.226000), state_ML_143; 139)
              | silent_transition(CONST_FLOAT(-4.244000), state_MR_144; 139)
              | silent_transition(CONST_FLOAT(-0.319000), state_D_145; 139)
              # h;

  state_IL_140 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_140; 140)
               | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_141; 140)
               | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_142; 140)
               | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_143; 140)
               | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_144; 140)
               | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_145; 140)
               # h;

  state_IR_141 = right_transition(CONST_FLOAT(-2.408000), state_IR_141, CHAR; 141)
               | right_transition(CONST_FLOAT(-0.496000), state_MP_142, CHAR; 141)
               | right_transition(CONST_FLOAT(-5.920000), state_ML_143, CHAR; 141)
               | right_transition(CONST_FLOAT(-4.087000), state_MR_144, CHAR; 141)
               | right_transition(CONST_FLOAT(-5.193000), state_D_145, CHAR; 141)
               # h;

  // node [ MATP 18 ]
  state_MP_142 = pair_transition(CONST_FLOAT(-11.233000), CHAR, state_IL_146, CHAR; 142)
               | pair_transition(CONST_FLOAT(-11.440000), CHAR, state_IR_147, CHAR; 142)
               | pair_transition(CONST_FLOAT(-0.005000), CHAR, state_ML_148, CHAR; 142)
               | pair_transition(CONST_FLOAT(-8.416000), CHAR, state_D_149, CHAR; 142)
               # h;

  state_ML_143 = left_transition(CONST_FLOAT(-3.758000), CHAR, state_IL_146; 143)
               | left_transition(CONST_FLOAT(-3.940000), CHAR, state_IR_147; 143)
               | left_transition(CONST_FLOAT(-0.507000), CHAR, state_ML_148; 143)
               | left_transition(CONST_FLOAT(-2.670000), CHAR, state_D_149; 143)
               # h;

  state_MR_144 = right_transition(CONST_FLOAT(-4.809000), state_IL_146, CHAR; 144)
               | right_transition(CONST_FLOAT(-3.838000), state_IR_147, CHAR; 144)
               | right_transition(CONST_FLOAT(-1.706000), state_ML_148, CHAR; 144)
               | right_transition(CONST_FLOAT(-0.766000), state_D_149, CHAR; 144)
               # h;

  state_D_145 = silent_transition(CONST_FLOAT(-4.568000), state_IL_146; 145)
              | silent_transition(CONST_FLOAT(-4.250000), state_IR_147; 145)
              | silent_transition(CONST_FLOAT(-2.265000), state_ML_148; 145)
              | silent_transition(CONST_FLOAT(-0.520000), state_D_149; 145)
              # h;

  state_IL_146 = left_transition(CONST_FLOAT(-1.686000), CHAR, state_IL_146; 146)
               | left_transition(CONST_FLOAT(-2.369000), CHAR, state_IR_147; 146)
               | left_transition(CONST_FLOAT(-1.117000), CHAR, state_ML_148; 146)
               | left_transition(CONST_FLOAT(-4.855000), CHAR, state_D_149; 146)
               # h;

  state_IR_147 = right_transition(CONST_FLOAT(-1.442000), state_IR_147, CHAR; 147)
               | right_transition(CONST_FLOAT(-0.798000), state_ML_148, CHAR; 147)
               | right_transition(CONST_FLOAT(-4.142000), state_D_149, CHAR; 147)
               # h;

  // node [ MATL 19 ]
  state_ML_148 = left_transition(CONST_FLOAT(-12.145000), CHAR, state_IL_150; 148)
               | left_transition(CONST_FLOAT(-0.014000), CHAR, state_ML_151; 148)
               | left_transition(CONST_FLOAT(-6.756000), CHAR, state_D_152; 148)
               # h;

  state_D_149 = silent_transition(CONST_FLOAT(-6.563000), state_IL_150; 149)
              | silent_transition(CONST_FLOAT(-2.076000), state_ML_151; 149)
              | silent_transition(CONST_FLOAT(-0.411000), state_D_152; 149)
              # h;

  state_IL_150 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_150; 150)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_151; 150)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_152; 150)
               # h;

  // node [ MATL 20 ]
  state_ML_151 = left_transition(CONST_FLOAT(-12.132000), CHAR, state_IL_153; 151)
               | left_transition(CONST_FLOAT(-0.253000), CHAR, state_ML_154; 151)
               | left_transition(CONST_FLOAT(-2.636000), CHAR, state_D_155; 151)
               # h;

  state_D_152 = silent_transition(CONST_FLOAT(-7.642000), state_IL_153; 152)
              | silent_transition(CONST_FLOAT(-3.155000), state_ML_154; 152)
              | silent_transition(CONST_FLOAT(-0.180000), state_D_155; 152)
              # h;

  state_IL_153 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_153; 153)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_154; 153)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_155; 153)
               # h;

  // node [ MATL 21 ]
  state_ML_154 = left_transition(CONST_FLOAT(-1.470000), CHAR, state_IL_156; 154)
               | left_transition(CONST_FLOAT(-0.709000), CHAR, state_ML_157; 154)
               | left_transition(CONST_FLOAT(-5.191000), CHAR, state_D_158; 154)
               # h;

  state_D_155 = silent_transition(CONST_FLOAT(-11.053000), state_IL_156; 155)
              | silent_transition(CONST_FLOAT(-0.280000), state_ML_157; 155)
              | silent_transition(CONST_FLOAT(-2.506000), state_D_158; 155)
              # h;

  state_IL_156 = left_transition(CONST_FLOAT(-1.948000), CHAR, state_IL_156; 156)
               | left_transition(CONST_FLOAT(-0.495000), CHAR, state_ML_157; 156)
               | left_transition(CONST_FLOAT(-5.006000), CHAR, state_D_158; 156)
               # h;

  // node [ MATL 22 ]
  state_ML_157 = left_transition(CONST_FLOAT(-12.057000), CHAR, state_IL_159; 157)
               | left_transition(CONST_FLOAT(-0.001000), CHAR, state_ML_160; 157)
               | left_transition(CONST_FLOAT(-10.711000), CHAR, state_D_161; 157)
               # h;

  state_D_158 = silent_transition(CONST_FLOAT(-9.663000), state_IL_159; 158)
              | silent_transition(CONST_FLOAT(-5.176000), state_ML_160; 158)
              | silent_transition(CONST_FLOAT(-0.042000), state_D_161; 158)
              # h;

  state_IL_159 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_159; 159)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_160; 159)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_161; 159)
               # h;

  // node [ MATL 23 ]
  state_ML_160 = left_transition(CONST_FLOAT(-12.057000), CHAR, state_IL_162; 160)
               | left_transition(CONST_FLOAT(-0.235000), CHAR, state_ML_163; 160)
               | left_transition(CONST_FLOAT(-2.736000), CHAR, state_D_164; 160)
               # h;

  state_D_161 = silent_transition(CONST_FLOAT(-9.663000), state_IL_162; 161)
              | silent_transition(CONST_FLOAT(-5.176000), state_ML_163; 161)
              | silent_transition(CONST_FLOAT(-0.042000), state_D_164; 161)
              # h;

  state_IL_162 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_162; 162)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_163; 162)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_164; 162)
               # h;

  // node [ MATL 24 ]
  state_ML_163 = left_transition(CONST_FLOAT(-0.840000), CHAR, state_IL_165; 163)
               | left_transition(CONST_FLOAT(-1.186000), CHAR, state_ML_166; 163)
               | left_transition(CONST_FLOAT(-9.087000), CHAR, state_D_167; 163)
               # h;

  state_D_164 = silent_transition(CONST_FLOAT(-8.061000), state_IL_165; 164)
              | silent_transition(CONST_FLOAT(-0.048000), state_ML_166; 164)
              | silent_transition(CONST_FLOAT(-5.093000), state_D_167; 164)
              # h;

  state_IL_165 = left_transition(CONST_FLOAT(-1.693000), CHAR, state_IL_165; 165)
               | left_transition(CONST_FLOAT(-0.554000), CHAR, state_ML_166; 165)
               | left_transition(CONST_FLOAT(-6.680000), CHAR, state_D_167; 165)
               # h;

  // node [ MATL 25 ]
  state_ML_166 = left_transition(CONST_FLOAT(0.000000), CHAR, state_E_169; 166)
               # h;

  state_D_167 = silent_transition(CONST_FLOAT(0.000000), state_E_169; 167)
              # h;

  state_IL_168 = left_transition(CONST_FLOAT(-1.823000), CHAR, state_IL_168; 168)
               | left_transition(CONST_FLOAT(-0.479000), CHAR, state_E_169; 168)
               # h;

  // node [ END 26 ]
  state_E_169 = nil(CONST_FLOAT(0.000000), EMPTY; 169)
              # h;

  // node [ BEGR 27 ]
  state_S_170 = silent_transition(CONST_FLOAT(-12.148000), state_IL_171; 170)
              | silent_transition(CONST_FLOAT(-0.015000), state_ML_172; 170)
              | silent_transition(CONST_FLOAT(-6.633000), state_D_173; 170)
              # h;

  state_IL_171 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_171; 171)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_172; 171)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_173; 171)
               # h;

  // node [ MATL 28 ]
  state_ML_172 = left_transition(CONST_FLOAT(-5.513000), CHAR, state_IL_174; 172)
               | left_transition(CONST_FLOAT(-0.041000), CHAR, state_MP_175; 172)
               | left_transition(CONST_FLOAT(-7.499000), CHAR, state_ML_176; 172)
               | left_transition(CONST_FLOAT(-11.106000), CHAR, state_MR_177; 172)
               | left_transition(CONST_FLOAT(-11.997000), CHAR, state_D_178; 172)
               # h;

  state_D_173 = silent_transition(CONST_FLOAT(-5.885000), state_IL_174; 173)
              | silent_transition(CONST_FLOAT(-0.367000), state_MP_175; 173)
              | silent_transition(CONST_FLOAT(-5.147000), state_ML_176; 173)
              | silent_transition(CONST_FLOAT(-3.521000), state_MR_177; 173)
              | silent_transition(CONST_FLOAT(-3.434000), state_D_178; 173)
              # h;

  state_IL_174 = left_transition(CONST_FLOAT(-3.373000), CHAR, state_IL_174; 174)
               | left_transition(CONST_FLOAT(-0.233000), CHAR, state_MP_175; 174)
               | left_transition(CONST_FLOAT(-5.052000), CHAR, state_ML_176; 174)
               | left_transition(CONST_FLOAT(-6.884000), CHAR, state_MR_177; 174)
               | left_transition(CONST_FLOAT(-6.158000), CHAR, state_D_178; 174)
               # h;

  // node [ MATP 29 ]
  state_MP_175 = pair_transition(CONST_FLOAT(-6.814000), CHAR, state_IL_179, CHAR; 175)
               | pair_transition(CONST_FLOAT(-6.792000), CHAR, state_IR_180, CHAR; 175)
               | pair_transition(CONST_FLOAT(-0.028000), CHAR, state_MP_181, CHAR; 175)
               | pair_transition(CONST_FLOAT(-10.827000), CHAR, state_ML_182, CHAR; 175)
               | pair_transition(CONST_FLOAT(-11.106000), CHAR, state_MR_183, CHAR; 175)
               | pair_transition(CONST_FLOAT(-11.501000), CHAR, state_D_184, CHAR; 175)
               # h;

  state_ML_176 = left_transition(CONST_FLOAT(-6.831000), CHAR, state_IL_179; 176)
               | left_transition(CONST_FLOAT(-7.177000), CHAR, state_IR_180; 176)
               | left_transition(CONST_FLOAT(-0.735000), CHAR, state_MP_181; 176)
               | left_transition(CONST_FLOAT(-1.586000), CHAR, state_ML_182; 176)
               | left_transition(CONST_FLOAT(-7.027000), CHAR, state_MR_183; 176)
               | left_transition(CONST_FLOAT(-4.556000), CHAR, state_D_184; 176)
               # h;

  state_MR_177 = right_transition(CONST_FLOAT(-6.988000), state_IL_179, CHAR; 177)
               | right_transition(CONST_FLOAT(-5.717000), state_IR_180, CHAR; 177)
               | right_transition(CONST_FLOAT(-1.625000), state_MP_181, CHAR; 177)
               | right_transition(CONST_FLOAT(-5.695000), state_ML_182, CHAR; 177)
               | right_transition(CONST_FLOAT(-0.829000), state_MR_183, CHAR; 177)
               | right_transition(CONST_FLOAT(-3.908000), state_D_184, CHAR; 177)
               # h;

  state_D_178 = silent_transition(CONST_FLOAT(-9.049000), state_IL_179; 178)
              | silent_transition(CONST_FLOAT(-7.747000), state_IR_180; 178)
              | silent_transition(CONST_FLOAT(-3.544000), state_MP_181; 178)
              | silent_transition(CONST_FLOAT(-4.226000), state_ML_182; 178)
              | silent_transition(CONST_FLOAT(-4.244000), state_MR_183; 178)
              | silent_transition(CONST_FLOAT(-0.319000), state_D_184; 178)
              # h;

  state_IL_179 = left_transition(CONST_FLOAT(-3.329000), CHAR, state_IL_179; 179)
               | left_transition(CONST_FLOAT(-3.592000), CHAR, state_IR_180; 179)
               | left_transition(CONST_FLOAT(-0.403000), CHAR, state_MP_181; 179)
               | left_transition(CONST_FLOAT(-5.247000), CHAR, state_ML_182; 179)
               | left_transition(CONST_FLOAT(-6.023000), CHAR, state_MR_183; 179)
               | left_transition(CONST_FLOAT(-5.684000), CHAR, state_D_184; 179)
               # h;

  state_IR_180 = right_transition(CONST_FLOAT(-2.914000), state_IR_180, CHAR; 180)
               | right_transition(CONST_FLOAT(-0.331000), state_MP_181, CHAR; 180)
               | right_transition(CONST_FLOAT(-6.425000), state_ML_182, CHAR; 180)
               | right_transition(CONST_FLOAT(-4.593000), state_MR_183, CHAR; 180)
               | right_transition(CONST_FLOAT(-5.699000), state_D_184, CHAR; 180)
               # h;

  // node [ MATP 30 ]
  state_MP_181 = pair_transition(CONST_FLOAT(-12.117000), CHAR, state_IL_185, CHAR; 181)
               | pair_transition(CONST_FLOAT(-12.057000), CHAR, state_IR_186, CHAR; 181)
               | pair_transition(CONST_FLOAT(-0.003000), CHAR, state_MP_187, CHAR; 181)
               | pair_transition(CONST_FLOAT(-10.833000), CHAR, state_ML_188, CHAR; 181)
               | pair_transition(CONST_FLOAT(-11.113000), CHAR, state_MR_189, CHAR; 181)
               | pair_transition(CONST_FLOAT(-11.508000), CHAR, state_D_190, CHAR; 181)
               # h;

  state_ML_182 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_185; 182)
               | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_186; 182)
               | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_187; 182)
               | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_188; 182)
               | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_189; 182)
               | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_190; 182)
               # h;

  state_MR_183 = right_transition(CONST_FLOAT(-6.988000), state_IL_185, CHAR; 183)
               | right_transition(CONST_FLOAT(-5.717000), state_IR_186, CHAR; 183)
               | right_transition(CONST_FLOAT(-1.625000), state_MP_187, CHAR; 183)
               | right_transition(CONST_FLOAT(-5.695000), state_ML_188, CHAR; 183)
               | right_transition(CONST_FLOAT(-0.829000), state_MR_189, CHAR; 183)
               | right_transition(CONST_FLOAT(-3.908000), state_D_190, CHAR; 183)
               # h;

  state_D_184 = silent_transition(CONST_FLOAT(-9.049000), state_IL_185; 184)
              | silent_transition(CONST_FLOAT(-7.747000), state_IR_186; 184)
              | silent_transition(CONST_FLOAT(-3.544000), state_MP_187; 184)
              | silent_transition(CONST_FLOAT(-4.226000), state_ML_188; 184)
              | silent_transition(CONST_FLOAT(-4.244000), state_MR_189; 184)
              | silent_transition(CONST_FLOAT(-0.319000), state_D_190; 184)
              # h;

  state_IL_185 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_185; 185)
               | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_186; 185)
               | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_187; 185)
               | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_188; 185)
               | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_189; 185)
               | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_190; 185)
               # h;

  state_IR_186 = right_transition(CONST_FLOAT(-2.408000), state_IR_186, CHAR; 186)
               | right_transition(CONST_FLOAT(-0.496000), state_MP_187, CHAR; 186)
               | right_transition(CONST_FLOAT(-5.920000), state_ML_188, CHAR; 186)
               | right_transition(CONST_FLOAT(-4.087000), state_MR_189, CHAR; 186)
               | right_transition(CONST_FLOAT(-5.193000), state_D_190, CHAR; 186)
               # h;

  // node [ MATP 31 ]
  state_MP_187 = pair_transition(CONST_FLOAT(-12.117000), CHAR, state_IL_191, CHAR; 187)
               | pair_transition(CONST_FLOAT(-12.057000), CHAR, state_IR_192, CHAR; 187)
               | pair_transition(CONST_FLOAT(-0.004000), CHAR, state_MP_193, CHAR; 187)
               | pair_transition(CONST_FLOAT(-10.833000), CHAR, state_ML_194, CHAR; 187)
               | pair_transition(CONST_FLOAT(-9.718000), CHAR, state_MR_195, CHAR; 187)
               | pair_transition(CONST_FLOAT(-11.508000), CHAR, state_D_196, CHAR; 187)
               # h;

  state_ML_188 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_191; 188)
               | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_192; 188)
               | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_193; 188)
               | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_194; 188)
               | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_195; 188)
               | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_196; 188)
               # h;

  state_MR_189 = right_transition(CONST_FLOAT(-6.988000), state_IL_191, CHAR; 189)
               | right_transition(CONST_FLOAT(-5.717000), state_IR_192, CHAR; 189)
               | right_transition(CONST_FLOAT(-1.625000), state_MP_193, CHAR; 189)
               | right_transition(CONST_FLOAT(-5.695000), state_ML_194, CHAR; 189)
               | right_transition(CONST_FLOAT(-0.829000), state_MR_195, CHAR; 189)
               | right_transition(CONST_FLOAT(-3.908000), state_D_196, CHAR; 189)
               # h;

  state_D_190 = silent_transition(CONST_FLOAT(-9.049000), state_IL_191; 190)
              | silent_transition(CONST_FLOAT(-7.747000), state_IR_192; 190)
              | silent_transition(CONST_FLOAT(-3.544000), state_MP_193; 190)
              | silent_transition(CONST_FLOAT(-4.226000), state_ML_194; 190)
              | silent_transition(CONST_FLOAT(-4.244000), state_MR_195; 190)
              | silent_transition(CONST_FLOAT(-0.319000), state_D_196; 190)
              # h;

  state_IL_191 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_191; 191)
               | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_192; 191)
               | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_193; 191)
               | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_194; 191)
               | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_195; 191)
               | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_196; 191)
               # h;

  state_IR_192 = right_transition(CONST_FLOAT(-2.408000), state_IR_192, CHAR; 192)
               | right_transition(CONST_FLOAT(-0.496000), state_MP_193, CHAR; 192)
               | right_transition(CONST_FLOAT(-5.920000), state_ML_194, CHAR; 192)
               | right_transition(CONST_FLOAT(-4.087000), state_MR_195, CHAR; 192)
               | right_transition(CONST_FLOAT(-5.193000), state_D_196, CHAR; 192)
               # h;

  // node [ MATP 32 ]
  state_MP_193 = pair_transition(CONST_FLOAT(-12.116000), CHAR, state_IL_197, CHAR; 193)
               | pair_transition(CONST_FLOAT(-12.056000), CHAR, state_IR_198, CHAR; 193)
               | pair_transition(CONST_FLOAT(-0.003000), CHAR, state_MP_199, CHAR; 193)
               | pair_transition(CONST_FLOAT(-10.832000), CHAR, state_ML_200, CHAR; 193)
               | pair_transition(CONST_FLOAT(-9.881000), CHAR, state_MR_201, CHAR; 193)
               | pair_transition(CONST_FLOAT(-11.507000), CHAR, state_D_202, CHAR; 193)
               # h;

  state_ML_194 = left_transition(CONST_FLOAT(-6.250000), CHAR, state_IL_197; 194)
               | left_transition(CONST_FLOAT(-6.596000), CHAR, state_IR_198; 194)
               | left_transition(CONST_FLOAT(-1.310000), CHAR, state_MP_199; 194)
               | left_transition(CONST_FLOAT(-1.005000), CHAR, state_ML_200; 194)
               | left_transition(CONST_FLOAT(-6.446000), CHAR, state_MR_201; 194)
               | left_transition(CONST_FLOAT(-3.975000), CHAR, state_D_202; 194)
               # h;

  state_MR_195 = right_transition(CONST_FLOAT(-7.083000), state_IL_197, CHAR; 195)
               | right_transition(CONST_FLOAT(-5.812000), state_IR_198, CHAR; 195)
               | right_transition(CONST_FLOAT(-1.444000), state_MP_199, CHAR; 195)
               | right_transition(CONST_FLOAT(-5.790000), state_ML_200, CHAR; 195)
               | right_transition(CONST_FLOAT(-0.924000), state_MR_201, CHAR; 195)
               | right_transition(CONST_FLOAT(-4.004000), state_D_202, CHAR; 195)
               # h;

  state_D_196 = silent_transition(CONST_FLOAT(-9.049000), state_IL_197; 196)
              | silent_transition(CONST_FLOAT(-7.747000), state_IR_198; 196)
              | silent_transition(CONST_FLOAT(-3.544000), state_MP_199; 196)
              | silent_transition(CONST_FLOAT(-4.226000), state_ML_200; 196)
              | silent_transition(CONST_FLOAT(-4.244000), state_MR_201; 196)
              | silent_transition(CONST_FLOAT(-0.319000), state_D_202; 196)
              # h;

  state_IL_197 = left_transition(CONST_FLOAT(-2.579000), CHAR, state_IL_197; 197)
               | left_transition(CONST_FLOAT(-2.842000), CHAR, state_IR_198; 197)
               | left_transition(CONST_FLOAT(-0.760000), CHAR, state_MP_199; 197)
               | left_transition(CONST_FLOAT(-4.497000), CHAR, state_ML_200; 197)
               | left_transition(CONST_FLOAT(-5.274000), CHAR, state_MR_201; 197)
               | left_transition(CONST_FLOAT(-4.934000), CHAR, state_D_202; 197)
               # h;

  state_IR_198 = right_transition(CONST_FLOAT(-2.408000), state_IR_198, CHAR; 198)
               | right_transition(CONST_FLOAT(-0.496000), state_MP_199, CHAR; 198)
               | right_transition(CONST_FLOAT(-5.920000), state_ML_200, CHAR; 198)
               | right_transition(CONST_FLOAT(-4.087000), state_MR_201, CHAR; 198)
               | right_transition(CONST_FLOAT(-5.193000), state_D_202, CHAR; 198)
               # h;

  // node [ MATP 33 ]
  state_MP_199 = pair_transition(CONST_FLOAT(-11.232000), CHAR, state_IL_203, CHAR; 199)
               | pair_transition(CONST_FLOAT(-11.439000), CHAR, state_IR_204, CHAR; 199)
               | pair_transition(CONST_FLOAT(-0.003000), CHAR, state_ML_205, CHAR; 199)
               | pair_transition(CONST_FLOAT(-9.853000), CHAR, state_D_206, CHAR; 199)
               # h;

  state_ML_200 = left_transition(CONST_FLOAT(-3.758000), CHAR, state_IL_203; 200)
               | left_transition(CONST_FLOAT(-3.940000), CHAR, state_IR_204; 200)
               | left_transition(CONST_FLOAT(-0.507000), CHAR, state_ML_205; 200)
               | left_transition(CONST_FLOAT(-2.670000), CHAR, state_D_206; 200)
               # h;

  state_MR_201 = right_transition(CONST_FLOAT(-4.901000), state_IL_203, CHAR; 201)
               | right_transition(CONST_FLOAT(-3.930000), state_IR_204, CHAR; 201)
               | right_transition(CONST_FLOAT(-1.519000), state_ML_205, CHAR; 201)
               | right_transition(CONST_FLOAT(-0.858000), state_D_206, CHAR; 201)
               # h;

  state_D_202 = silent_transition(CONST_FLOAT(-4.568000), state_IL_203; 202)
              | silent_transition(CONST_FLOAT(-4.250000), state_IR_204; 202)
              | silent_transition(CONST_FLOAT(-2.265000), state_ML_205; 202)
              | silent_transition(CONST_FLOAT(-0.520000), state_D_206; 202)
              # h;

  state_IL_203 = left_transition(CONST_FLOAT(-1.686000), CHAR, state_IL_203; 203)
               | left_transition(CONST_FLOAT(-2.369000), CHAR, state_IR_204; 203)
               | left_transition(CONST_FLOAT(-1.117000), CHAR, state_ML_205; 203)
               | left_transition(CONST_FLOAT(-4.855000), CHAR, state_D_206; 203)
               # h;

  state_IR_204 = right_transition(CONST_FLOAT(-1.442000), state_IR_204, CHAR; 204)
               | right_transition(CONST_FLOAT(-0.798000), state_ML_205, CHAR; 204)
               | right_transition(CONST_FLOAT(-4.142000), state_D_206, CHAR; 204)
               # h;

  // node [ MATL 34 ]
  state_ML_205 = left_transition(CONST_FLOAT(-6.418000), CHAR, state_IL_207; 205)
               | left_transition(CONST_FLOAT(-0.018000), CHAR, state_ML_208; 205)
               | left_transition(CONST_FLOAT(-10.802000), CHAR, state_D_209; 205)
               # h;

  state_D_206 = silent_transition(CONST_FLOAT(-6.174000), state_IL_207; 206)
              | silent_transition(CONST_FLOAT(-1.687000), state_ML_208; 206)
              | silent_transition(CONST_FLOAT(-0.566000), state_D_209; 206)
              # h;

  state_IL_207 = left_transition(CONST_FLOAT(-2.011000), CHAR, state_IL_207; 207)
               | left_transition(CONST_FLOAT(-0.486000), CHAR, state_ML_208; 207)
               | left_transition(CONST_FLOAT(-4.712000), CHAR, state_D_209; 207)
               # h;

  // node [ MATL 35 ]
  state_ML_208 = left_transition(CONST_FLOAT(-12.148000), CHAR, state_IL_210; 208)
               | left_transition(CONST_FLOAT(-0.001000), CHAR, state_ML_211; 208)
               | left_transition(CONST_FLOAT(-10.802000), CHAR, state_D_212; 208)
               # h;

  state_D_209 = silent_transition(CONST_FLOAT(-6.174000), state_IL_210; 209)
              | silent_transition(CONST_FLOAT(-1.687000), state_ML_211; 209)
              | silent_transition(CONST_FLOAT(-0.566000), state_D_212; 209)
              # h;

  state_IL_210 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_210; 210)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_211; 210)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_212; 210)
               # h;

  // node [ MATL 36 ]
  state_ML_211 = left_transition(CONST_FLOAT(-12.148000), CHAR, state_IL_213; 211)
               | left_transition(CONST_FLOAT(-0.001000), CHAR, state_ML_214; 211)
               | left_transition(CONST_FLOAT(-10.802000), CHAR, state_D_215; 211)
               # h;

  state_D_212 = silent_transition(CONST_FLOAT(-6.174000), state_IL_213; 212)
              | silent_transition(CONST_FLOAT(-1.687000), state_ML_214; 212)
              | silent_transition(CONST_FLOAT(-0.566000), state_D_215; 212)
              # h;

  state_IL_213 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_213; 213)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_214; 213)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_215; 213)
               # h;

  // node [ MATL 37 ]
  state_ML_214 = left_transition(CONST_FLOAT(-12.148000), CHAR, state_IL_216; 214)
               | left_transition(CONST_FLOAT(-0.001000), CHAR, state_ML_217; 214)
               | left_transition(CONST_FLOAT(-10.802000), CHAR, state_D_218; 214)
               # h;

  state_D_215 = silent_transition(CONST_FLOAT(-6.174000), state_IL_216; 215)
              | silent_transition(CONST_FLOAT(-1.687000), state_ML_217; 215)
              | silent_transition(CONST_FLOAT(-0.566000), state_D_218; 215)
              # h;

  state_IL_216 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_216; 216)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_217; 216)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_218; 216)
               # h;

  // node [ MATL 38 ]
  state_ML_217 = left_transition(CONST_FLOAT(-12.148000), CHAR, state_IL_219; 217)
               | left_transition(CONST_FLOAT(-0.001000), CHAR, state_ML_220; 217)
               | left_transition(CONST_FLOAT(-10.802000), CHAR, state_D_221; 217)
               # h;

  state_D_218 = silent_transition(CONST_FLOAT(-6.174000), state_IL_219; 218)
              | silent_transition(CONST_FLOAT(-1.687000), state_ML_220; 218)
              | silent_transition(CONST_FLOAT(-0.566000), state_D_221; 218)
              # h;

  state_IL_219 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_219; 219)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_220; 219)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_221; 219)
               # h;

  // node [ MATL 39 ]
  state_ML_220 = left_transition(CONST_FLOAT(-12.148000), CHAR, state_IL_222; 220)
               | left_transition(CONST_FLOAT(-0.001000), CHAR, state_ML_223; 220)
               | left_transition(CONST_FLOAT(-10.802000), CHAR, state_D_224; 220)
               # h;

  state_D_221 = silent_transition(CONST_FLOAT(-6.174000), state_IL_222; 221)
              | silent_transition(CONST_FLOAT(-1.687000), state_ML_223; 221)
              | silent_transition(CONST_FLOAT(-0.566000), state_D_224; 221)
              # h;

  state_IL_222 = left_transition(CONST_FLOAT(-1.442000), CHAR, state_IL_222; 222)
               | left_transition(CONST_FLOAT(-0.798000), CHAR, state_ML_223; 222)
               | left_transition(CONST_FLOAT(-4.142000), CHAR, state_D_224; 222)
               # h;

  // node [ MATL 40 ]
  state_ML_223 = left_transition(CONST_FLOAT(0.000000), CHAR, state_E_226; 223)
               # h;

  state_D_224 = silent_transition(CONST_FLOAT(0.000000), state_E_226; 224)
              # h;

  state_IL_225 = left_transition(CONST_FLOAT(-1.823000), CHAR, state_IL_225; 225)
               | left_transition(CONST_FLOAT(-0.479000), CHAR, state_E_226; 225)
               # h;

  // node [ END 41 ]
  state_E_226 = nil(CONST_FLOAT(0.000000), EMPTY; 226)
              # h;

}

instance count = gra_cm(alg_count);
instance enumcyk = gra_cm(alg_enum * alg_cyk);
instance cykenum = gra_cm(alg_cyk * alg_enum);
instance inside = gra_cm(alg_inside);
    
signature sig_cm(alphabet, answer) {
  answer silent_transition(float, answer);
  answer transition(float, alphabet, answer; int);
  answer pair_transition(float, alphabet, answer, alphabet; int);
  answer bifurcation_transition(float, float, answer, answer; int);
  answer nil(float, void; int);
  choice [answer] h([answer]);
}


algebra alg_enum auto enum;

algebra alg_count auto count;

grammar gra_cm uses sig_cm(axiom = state_S_0) {
  // node [ ROOT 0 ]
  state_S_0 = silent_transition(CONST_FLOAT(-10.774000), state_IL_1)
            | silent_transition(CONST_FLOAT(-10.713000), state_IR_2)
            | silent_transition(CONST_FLOAT(-0.007000), state_MP_3)
            | silent_transition(CONST_FLOAT(-9.490000), state_ML_4)
            | silent_transition(CONST_FLOAT(-9.770000), state_MR_5)
            | silent_transition(CONST_FLOAT(-10.165000), state_D_6)
            # h;

  state_IL_1 = transition(CONST_FLOAT(-2.579000), CHAR, state_IL_1; 1)
             | transition(CONST_FLOAT(-2.842000), CHAR, state_IR_2; 1)
             | transition(CONST_FLOAT(-0.760000), CHAR, state_MP_3; 1)
             | transition(CONST_FLOAT(-4.497000), CHAR, state_ML_4; 1)
             | transition(CONST_FLOAT(-5.274000), CHAR, state_MR_5; 1)
             | transition(CONST_FLOAT(-4.934000), CHAR, state_D_6; 1)
             # h;

  state_IR_2 = transition(CONST_FLOAT(-2.408000), CHAR, state_IR_2; 2)
             | transition(CONST_FLOAT(-0.496000), CHAR, state_MP_3; 2)
             | transition(CONST_FLOAT(-5.920000), CHAR, state_ML_4; 2)
             | transition(CONST_FLOAT(-4.087000), CHAR, state_MR_5; 2)
             | transition(CONST_FLOAT(-5.193000), CHAR, state_D_6; 2)
             # h;

  // node [ MATP 1 ]
  state_MP_3 = pair_transition(CONST_FLOAT(-10.774000), CHAR, state_IL_7, CHAR; 3)
             | pair_transition(CONST_FLOAT(-10.713000), CHAR, state_IR_8, CHAR; 3)
             | pair_transition(CONST_FLOAT(-0.007000), CHAR, state_MP_9, CHAR; 3)
             | pair_transition(CONST_FLOAT(-9.490000), CHAR, state_ML_10, CHAR; 3)
             | pair_transition(CONST_FLOAT(-9.770000), CHAR, state_MR_11, CHAR; 3)
             | pair_transition(CONST_FLOAT(-10.165000), CHAR, state_D_12, CHAR; 3)
             # h;

  state_ML_4 = transition(CONST_FLOAT(-6.250000), CHAR, state_IL_7; 4)
             | transition(CONST_FLOAT(-6.596000), CHAR, state_IR_8; 4)
             | transition(CONST_FLOAT(-1.310000), CHAR, state_MP_9; 4)
             | transition(CONST_FLOAT(-1.005000), CHAR, state_ML_10; 4)
             | transition(CONST_FLOAT(-6.446000), CHAR, state_MR_11; 4)
             | transition(CONST_FLOAT(-3.975000), CHAR, state_D_12; 4)
             # h;

  state_MR_5 = transition(CONST_FLOAT(-6.988000), CHAR, state_IL_7; 5)
             | transition(CONST_FLOAT(-5.717000), CHAR, state_IR_8; 5)
             | transition(CONST_FLOAT(-1.625000), CHAR, state_MP_9; 5)
             | transition(CONST_FLOAT(-5.695000), CHAR, state_ML_10; 5)
             | transition(CONST_FLOAT(-0.829000), CHAR, state_MR_11; 5)
             | transition(CONST_FLOAT(-3.908000), CHAR, state_D_12; 5)
             # h;

  state_D_6 = silent_transition(CONST_FLOAT(-9.049000), state_IL_7)
            | silent_transition(CONST_FLOAT(-7.747000), state_IR_8)
            | silent_transition(CONST_FLOAT(-3.544000), state_MP_9)
            | silent_transition(CONST_FLOAT(-4.226000), state_ML_10)
            | silent_transition(CONST_FLOAT(-4.244000), state_MR_11)
            | silent_transition(CONST_FLOAT(-0.319000), state_D_12)
            # h;

  state_IL_7 = transition(CONST_FLOAT(-2.579000), CHAR, state_IL_7; 7)
             | transition(CONST_FLOAT(-2.842000), CHAR, state_IR_8; 7)
             | transition(CONST_FLOAT(-0.760000), CHAR, state_MP_9; 7)
             | transition(CONST_FLOAT(-4.497000), CHAR, state_ML_10; 7)
             | transition(CONST_FLOAT(-5.274000), CHAR, state_MR_11; 7)
             | transition(CONST_FLOAT(-4.934000), CHAR, state_D_12; 7)
             # h;

  state_IR_8 = transition(CONST_FLOAT(-2.408000), CHAR, state_IR_8; 8)
             | transition(CONST_FLOAT(-0.496000), CHAR, state_MP_9; 8)
             | transition(CONST_FLOAT(-5.920000), CHAR, state_ML_10; 8)
             | transition(CONST_FLOAT(-4.087000), CHAR, state_MR_11; 8)
             | transition(CONST_FLOAT(-5.193000), CHAR, state_D_12; 8)
             # h;

  // node [ MATP 2 ]
  state_MP_9 = pair_transition(CONST_FLOAT(-10.774000), CHAR, state_IL_13, CHAR; 9)
             | pair_transition(CONST_FLOAT(-10.713000), CHAR, state_IR_14, CHAR; 9)
             | pair_transition(CONST_FLOAT(-0.007000), CHAR, state_MP_15, CHAR; 9)
             | pair_transition(CONST_FLOAT(-9.490000), CHAR, state_ML_16, CHAR; 9)
             | pair_transition(CONST_FLOAT(-9.770000), CHAR, state_MR_17, CHAR; 9)
             | pair_transition(CONST_FLOAT(-10.165000), CHAR, state_D_18, CHAR; 9)
             # h;

  state_ML_10 = transition(CONST_FLOAT(-6.250000), CHAR, state_IL_13; 10)
              | transition(CONST_FLOAT(-6.596000), CHAR, state_IR_14; 10)
              | transition(CONST_FLOAT(-1.310000), CHAR, state_MP_15; 10)
              | transition(CONST_FLOAT(-1.005000), CHAR, state_ML_16; 10)
              | transition(CONST_FLOAT(-6.446000), CHAR, state_MR_17; 10)
              | transition(CONST_FLOAT(-3.975000), CHAR, state_D_18; 10)
              # h;

  state_MR_11 = transition(CONST_FLOAT(-6.988000), CHAR, state_IL_13; 11)
              | transition(CONST_FLOAT(-5.717000), CHAR, state_IR_14; 11)
              | transition(CONST_FLOAT(-1.625000), CHAR, state_MP_15; 11)
              | transition(CONST_FLOAT(-5.695000), CHAR, state_ML_16; 11)
              | transition(CONST_FLOAT(-0.829000), CHAR, state_MR_17; 11)
              | transition(CONST_FLOAT(-3.908000), CHAR, state_D_18; 11)
              # h;

  state_D_12 = silent_transition(CONST_FLOAT(-9.049000), state_IL_13)
             | silent_transition(CONST_FLOAT(-7.747000), state_IR_14)
             | silent_transition(CONST_FLOAT(-3.544000), state_MP_15)
             | silent_transition(CONST_FLOAT(-4.226000), state_ML_16)
             | silent_transition(CONST_FLOAT(-4.244000), state_MR_17)
             | silent_transition(CONST_FLOAT(-0.319000), state_D_18)
             # h;

  state_IL_13 = transition(CONST_FLOAT(-2.579000), CHAR, state_IL_13; 13)
              | transition(CONST_FLOAT(-2.842000), CHAR, state_IR_14; 13)
              | transition(CONST_FLOAT(-0.760000), CHAR, state_MP_15; 13)
              | transition(CONST_FLOAT(-4.497000), CHAR, state_ML_16; 13)
              | transition(CONST_FLOAT(-5.274000), CHAR, state_MR_17; 13)
              | transition(CONST_FLOAT(-4.934000), CHAR, state_D_18; 13)
              # h;

  state_IR_14 = transition(CONST_FLOAT(-2.408000), CHAR, state_IR_14; 14)
              | transition(CONST_FLOAT(-0.496000), CHAR, state_MP_15; 14)
              | transition(CONST_FLOAT(-5.920000), CHAR, state_ML_16; 14)
              | transition(CONST_FLOAT(-4.087000), CHAR, state_MR_17; 14)
              | transition(CONST_FLOAT(-5.193000), CHAR, state_D_18; 14)
              # h;

  // node [ MATP 3 ]
  state_MP_15 = pair_transition(CONST_FLOAT(-10.774000), CHAR, state_IL_19, CHAR; 15)
              | pair_transition(CONST_FLOAT(-10.713000), CHAR, state_IR_20, CHAR; 15)
              | pair_transition(CONST_FLOAT(-0.007000), CHAR, state_MP_21, CHAR; 15)
              | pair_transition(CONST_FLOAT(-9.490000), CHAR, state_ML_22, CHAR; 15)
              | pair_transition(CONST_FLOAT(-9.770000), CHAR, state_MR_23, CHAR; 15)
              | pair_transition(CONST_FLOAT(-10.165000), CHAR, state_D_24, CHAR; 15)
              # h;

  state_ML_16 = transition(CONST_FLOAT(-6.250000), CHAR, state_IL_19; 16)
              | transition(CONST_FLOAT(-6.596000), CHAR, state_IR_20; 16)
              | transition(CONST_FLOAT(-1.310000), CHAR, state_MP_21; 16)
              | transition(CONST_FLOAT(-1.005000), CHAR, state_ML_22; 16)
              | transition(CONST_FLOAT(-6.446000), CHAR, state_MR_23; 16)
              | transition(CONST_FLOAT(-3.975000), CHAR, state_D_24; 16)
              # h;

  state_MR_17 = transition(CONST_FLOAT(-6.988000), CHAR, state_IL_19; 17)
              | transition(CONST_FLOAT(-5.717000), CHAR, state_IR_20; 17)
              | transition(CONST_FLOAT(-1.625000), CHAR, state_MP_21; 17)
              | transition(CONST_FLOAT(-5.695000), CHAR, state_ML_22; 17)
              | transition(CONST_FLOAT(-0.829000), CHAR, state_MR_23; 17)
              | transition(CONST_FLOAT(-3.908000), CHAR, state_D_24; 17)
              # h;

  state_D_18 = silent_transition(CONST_FLOAT(-9.049000), state_IL_19)
             | silent_transition(CONST_FLOAT(-7.747000), state_IR_20)
             | silent_transition(CONST_FLOAT(-3.544000), state_MP_21)
             | silent_transition(CONST_FLOAT(-4.226000), state_ML_22)
             | silent_transition(CONST_FLOAT(-4.244000), state_MR_23)
             | silent_transition(CONST_FLOAT(-0.319000), state_D_24)
             # h;

  state_IL_19 = transition(CONST_FLOAT(-2.579000), CHAR, state_IL_19; 19)
              | transition(CONST_FLOAT(-2.842000), CHAR, state_IR_20; 19)
              | transition(CONST_FLOAT(-0.760000), CHAR, state_MP_21; 19)
              | transition(CONST_FLOAT(-4.497000), CHAR, state_ML_22; 19)
              | transition(CONST_FLOAT(-5.274000), CHAR, state_MR_23; 19)
              | transition(CONST_FLOAT(-4.934000), CHAR, state_D_24; 19)
              # h;

  state_IR_20 = transition(CONST_FLOAT(-2.408000), CHAR, state_IR_20; 20)
              | transition(CONST_FLOAT(-0.496000), CHAR, state_MP_21; 20)
              | transition(CONST_FLOAT(-5.920000), CHAR, state_ML_22; 20)
              | transition(CONST_FLOAT(-4.087000), CHAR, state_MR_23; 20)
              | transition(CONST_FLOAT(-5.193000), CHAR, state_D_24; 20)
              # h;

  // node [ MATP 4 ]
  state_MP_21 = pair_transition(CONST_FLOAT(-9.505000), CHAR, state_IL_25, CHAR; 21)
              | pair_transition(CONST_FLOAT(-10.750000), CHAR, state_IR_26, CHAR; 21)
              | pair_transition(CONST_FLOAT(-0.013000), CHAR, state_MR_27, CHAR; 21)
              | pair_transition(CONST_FLOAT(-7.204000), CHAR, state_D_28, CHAR; 21)
              # h;

  state_ML_22 = transition(CONST_FLOAT(-2.408000), CHAR, state_IL_25; 22)
              | transition(CONST_FLOAT(-4.532000), CHAR, state_IR_26; 22)
              | transition(CONST_FLOAT(-1.293000), CHAR, state_MR_27; 22)
              | transition(CONST_FLOAT(-1.473000), CHAR, state_D_28; 22)
              # h;

  state_MR_23 = transition(CONST_FLOAT(-4.102000), CHAR, state_IL_25; 23)
              | transition(CONST_FLOAT(-12.528000), CHAR, state_IR_26; 23)
              | transition(CONST_FLOAT(-0.390000), CHAR, state_MR_27; 23)
              | transition(CONST_FLOAT(-2.485000), CHAR, state_D_28; 23)
              # h;

  state_D_24 = silent_transition(CONST_FLOAT(-12.737000), state_IL_25)
             | silent_transition(CONST_FLOAT(-14.007000), state_IR_26)
             | silent_transition(CONST_FLOAT(-2.036000), state_MR_27)
             | silent_transition(CONST_FLOAT(-0.404000), state_D_28)
             # h;

  state_IL_25 = transition(CONST_FLOAT(-2.817000), CHAR, state_IL_25; 25)
              | transition(CONST_FLOAT(-4.319000), CHAR, state_IR_26; 25)
              | transition(CONST_FLOAT(-0.613000), CHAR, state_MR_27; 25)
              | transition(CONST_FLOAT(-2.698000), CHAR, state_D_28; 25)
              # h;

  state_IR_26 = transition(CONST_FLOAT(-1.925000), CHAR, state_IR_26; 26)
              | transition(CONST_FLOAT(-0.554000), CHAR, state_MR_27; 26)
              | transition(CONST_FLOAT(-4.164000), CHAR, state_D_28; 26)
              # h;

  // node [ MATR 5 ]
  state_MR_27 = transition(CONST_FLOAT(-9.584000), CHAR, state_IR_29; 27)
              | transition(CONST_FLOAT(-0.007000), CHAR, state_MP_30; 27)
              | transition(CONST_FLOAT(-9.399000), CHAR, state_ML_31; 27)
              | transition(CONST_FLOAT(-9.611000), CHAR, state_MR_32; 27)
              | transition(CONST_FLOAT(-10.503000), CHAR, state_D_33; 27)
              # h;

  state_D_28 = silent_transition(CONST_FLOAT(-5.352000), state_IR_29)
             | silent_transition(CONST_FLOAT(-0.707000), state_MP_30)
             | silent_transition(CONST_FLOAT(-2.978000), state_ML_31)
             | silent_transition(CONST_FLOAT(-4.409000), state_MR_32)
             | silent_transition(CONST_FLOAT(-2.404000), state_D_33)
             # h;

  state_IR_29 = transition(CONST_FLOAT(-2.408000), CHAR, state_IR_29; 29)
              | transition(CONST_FLOAT(-0.496000), CHAR, state_MP_30; 29)
              | transition(CONST_FLOAT(-5.920000), CHAR, state_ML_31; 29)
              | transition(CONST_FLOAT(-4.087000), CHAR, state_MR_32; 29)
              | transition(CONST_FLOAT(-5.193000), CHAR, state_D_33; 29)
              # h;

  // node [ MATP 6 ]
  state_MP_30 = pair_transition(CONST_FLOAT(-10.774000), CHAR, state_IL_34, CHAR; 30)
              | pair_transition(CONST_FLOAT(-10.713000), CHAR, state_IR_35, CHAR; 30)
              | pair_transition(CONST_FLOAT(-0.007000), CHAR, state_MP_36, CHAR; 30)
              | pair_transition(CONST_FLOAT(-9.490000), CHAR, state_ML_37, CHAR; 30)
              | pair_transition(CONST_FLOAT(-9.770000), CHAR, state_MR_38, CHAR; 30)
              | pair_transition(CONST_FLOAT(-10.165000), CHAR, state_D_39, CHAR; 30)
              # h;

  state_ML_31 = transition(CONST_FLOAT(-6.250000), CHAR, state_IL_34; 31)
              | transition(CONST_FLOAT(-6.596000), CHAR, state_IR_35; 31)
              | transition(CONST_FLOAT(-1.310000), CHAR, state_MP_36; 31)
              | transition(CONST_FLOAT(-1.005000), CHAR, state_ML_37; 31)
              | transition(CONST_FLOAT(-6.446000), CHAR, state_MR_38; 31)
              | transition(CONST_FLOAT(-3.975000), CHAR, state_D_39; 31)
              # h;

  state_MR_32 = transition(CONST_FLOAT(-6.988000), CHAR, state_IL_34; 32)
              | transition(CONST_FLOAT(-5.717000), CHAR, state_IR_35; 32)
              | transition(CONST_FLOAT(-1.625000), CHAR, state_MP_36; 32)
              | transition(CONST_FLOAT(-5.695000), CHAR, state_ML_37; 32)
              | transition(CONST_FLOAT(-0.829000), CHAR, state_MR_38; 32)
              | transition(CONST_FLOAT(-3.908000), CHAR, state_D_39; 32)
              # h;

  state_D_33 = silent_transition(CONST_FLOAT(-9.049000), state_IL_34)
             | silent_transition(CONST_FLOAT(-7.747000), state_IR_35)
             | silent_transition(CONST_FLOAT(-3.544000), state_MP_36)
             | silent_transition(CONST_FLOAT(-4.226000), state_ML_37)
             | silent_transition(CONST_FLOAT(-4.244000), state_MR_38)
             | silent_transition(CONST_FLOAT(-0.319000), state_D_39)
             # h;

  state_IL_34 = transition(CONST_FLOAT(-2.579000), CHAR, state_IL_34; 34)
              | transition(CONST_FLOAT(-2.842000), CHAR, state_IR_35; 34)
              | transition(CONST_FLOAT(-0.760000), CHAR, state_MP_36; 34)
              | transition(CONST_FLOAT(-4.497000), CHAR, state_ML_37; 34)
              | transition(CONST_FLOAT(-5.274000), CHAR, state_MR_38; 34)
              | transition(CONST_FLOAT(-4.934000), CHAR, state_D_39; 34)
              # h;

  state_IR_35 = transition(CONST_FLOAT(-2.408000), CHAR, state_IR_35; 35)
              | transition(CONST_FLOAT(-0.496000), CHAR, state_MP_36; 35)
              | transition(CONST_FLOAT(-5.920000), CHAR, state_ML_37; 35)
              | transition(CONST_FLOAT(-4.087000), CHAR, state_MR_38; 35)
              | transition(CONST_FLOAT(-5.193000), CHAR, state_D_39; 35)
              # h;

  // node [ MATP 7 ]
  state_MP_36 = pair_transition(CONST_FLOAT(-10.774000), CHAR, state_IL_40, CHAR; 36)
              | pair_transition(CONST_FLOAT(-10.713000), CHAR, state_IR_41, CHAR; 36)
              | pair_transition(CONST_FLOAT(-0.007000), CHAR, state_MP_42, CHAR; 36)
              | pair_transition(CONST_FLOAT(-9.490000), CHAR, state_ML_43, CHAR; 36)
              | pair_transition(CONST_FLOAT(-9.770000), CHAR, state_MR_44, CHAR; 36)
              | pair_transition(CONST_FLOAT(-10.165000), CHAR, state_D_45, CHAR; 36)
              # h;

  state_ML_37 = transition(CONST_FLOAT(-6.250000), CHAR, state_IL_40; 37)
              | transition(CONST_FLOAT(-6.596000), CHAR, state_IR_41; 37)
              | transition(CONST_FLOAT(-1.310000), CHAR, state_MP_42; 37)
              | transition(CONST_FLOAT(-1.005000), CHAR, state_ML_43; 37)
              | transition(CONST_FLOAT(-6.446000), CHAR, state_MR_44; 37)
              | transition(CONST_FLOAT(-3.975000), CHAR, state_D_45; 37)
              # h;

  state_MR_38 = transition(CONST_FLOAT(-6.988000), CHAR, state_IL_40; 38)
              | transition(CONST_FLOAT(-5.717000), CHAR, state_IR_41; 38)
              | transition(CONST_FLOAT(-1.625000), CHAR, state_MP_42; 38)
              | transition(CONST_FLOAT(-5.695000), CHAR, state_ML_43; 38)
              | transition(CONST_FLOAT(-0.829000), CHAR, state_MR_44; 38)
              | transition(CONST_FLOAT(-3.908000), CHAR, state_D_45; 38)
              # h;

  state_D_39 = silent_transition(CONST_FLOAT(-9.049000), state_IL_40)
             | silent_transition(CONST_FLOAT(-7.747000), state_IR_41)
             | silent_transition(CONST_FLOAT(-3.544000), state_MP_42)
             | silent_transition(CONST_FLOAT(-4.226000), state_ML_43)
             | silent_transition(CONST_FLOAT(-4.244000), state_MR_44)
             | silent_transition(CONST_FLOAT(-0.319000), state_D_45)
             # h;

  state_IL_40 = transition(CONST_FLOAT(-2.579000), CHAR, state_IL_40; 40)
              | transition(CONST_FLOAT(-2.842000), CHAR, state_IR_41; 40)
              | transition(CONST_FLOAT(-0.760000), CHAR, state_MP_42; 40)
              | transition(CONST_FLOAT(-4.497000), CHAR, state_ML_43; 40)
              | transition(CONST_FLOAT(-5.274000), CHAR, state_MR_44; 40)
              | transition(CONST_FLOAT(-4.934000), CHAR, state_D_45; 40)
              # h;

  state_IR_41 = transition(CONST_FLOAT(-2.408000), CHAR, state_IR_41; 41)
              | transition(CONST_FLOAT(-0.496000), CHAR, state_MP_42; 41)
              | transition(CONST_FLOAT(-5.920000), CHAR, state_ML_43; 41)
              | transition(CONST_FLOAT(-4.087000), CHAR, state_MR_44; 41)
              | transition(CONST_FLOAT(-5.193000), CHAR, state_D_45; 41)
              # h;

  // node [ MATP 8 ]
  state_MP_42 = pair_transition(CONST_FLOAT(-9.692000), CHAR, state_IL_46, CHAR; 42)
              | pair_transition(CONST_FLOAT(-9.899000), CHAR, state_IR_47, CHAR; 42)
              | pair_transition(CONST_FLOAT(-0.008000), CHAR, state_ML_48, CHAR; 42)
              | pair_transition(CONST_FLOAT(-8.313000), CHAR, state_D_49, CHAR; 42)
              # h;

  state_ML_43 = transition(CONST_FLOAT(-3.758000), CHAR, state_IL_46; 43)
              | transition(CONST_FLOAT(-3.940000), CHAR, state_IR_47; 43)
              | transition(CONST_FLOAT(-0.507000), CHAR, state_ML_48; 43)
              | transition(CONST_FLOAT(-2.670000), CHAR, state_D_49; 43)
              # h;

  state_MR_44 = transition(CONST_FLOAT(-4.809000), CHAR, state_IL_46; 44)
              | transition(CONST_FLOAT(-3.838000), CHAR, state_IR_47; 44)
              | transition(CONST_FLOAT(-1.706000), CHAR, state_ML_48; 44)
              | transition(CONST_FLOAT(-0.766000), CHAR, state_D_49; 44)
              # h;

  state_D_45 = silent_transition(CONST_FLOAT(-4.568000), state_IL_46)
             | silent_transition(CONST_FLOAT(-4.250000), state_IR_47)
             | silent_transition(CONST_FLOAT(-2.265000), state_ML_48)
             | silent_transition(CONST_FLOAT(-0.520000), state_D_49)
             # h;

  state_IL_46 = transition(CONST_FLOAT(-1.686000), CHAR, state_IL_46; 46)
              | transition(CONST_FLOAT(-2.369000), CHAR, state_IR_47; 46)
              | transition(CONST_FLOAT(-1.117000), CHAR, state_ML_48; 46)
              | transition(CONST_FLOAT(-4.855000), CHAR, state_D_49; 46)
              # h;

  state_IR_47 = transition(CONST_FLOAT(-1.442000), CHAR, state_IR_47; 47)
              | transition(CONST_FLOAT(-0.798000), CHAR, state_ML_48; 47)
              | transition(CONST_FLOAT(-4.142000), CHAR, state_D_49; 47)
              # h;

  // node [ MATL 9 ]
  state_ML_48 = transition(CONST_FLOAT(-10.618000), CHAR, state_IL_50; 48)
              | transition(CONST_FLOAT(-0.003000), CHAR, state_ML_51; 48)
              | transition(CONST_FLOAT(-9.272000), CHAR, state_D_52; 48)
              # h;

  state_D_49 = silent_transition(CONST_FLOAT(-6.174000), state_IL_50)
             | silent_transition(CONST_FLOAT(-1.687000), state_ML_51)
             | silent_transition(CONST_FLOAT(-0.566000), state_D_52)
             # h;

  state_IL_50 = transition(CONST_FLOAT(-1.442000), CHAR, state_IL_50; 50)
              | transition(CONST_FLOAT(-0.798000), CHAR, state_ML_51; 50)
              | transition(CONST_FLOAT(-4.142000), CHAR, state_D_52; 50)
              # h;

  // node [ MATL 10 ]
  state_ML_51 = transition(CONST_FLOAT(-10.618000), CHAR, state_IL_53; 51)
              | transition(CONST_FLOAT(-0.003000), CHAR, state_ML_54; 51)
              | transition(CONST_FLOAT(-9.272000), CHAR, state_D_55; 51)
              # h;

  state_D_52 = silent_transition(CONST_FLOAT(-6.174000), state_IL_53)
             | silent_transition(CONST_FLOAT(-1.687000), state_ML_54)
             | silent_transition(CONST_FLOAT(-0.566000), state_D_55)
             # h;

  state_IL_53 = transition(CONST_FLOAT(-1.442000), CHAR, state_IL_53; 53)
              | transition(CONST_FLOAT(-0.798000), CHAR, state_ML_54; 53)
              | transition(CONST_FLOAT(-4.142000), CHAR, state_D_55; 53)
              # h;

  // node [ MATL 11 ]
  state_ML_54 = transition(CONST_FLOAT(-10.618000), CHAR, state_IL_56; 54)
              | transition(CONST_FLOAT(-0.003000), CHAR, state_ML_57; 54)
              | transition(CONST_FLOAT(-9.272000), CHAR, state_D_58; 54)
              # h;

  state_D_55 = silent_transition(CONST_FLOAT(-6.174000), state_IL_56)
             | silent_transition(CONST_FLOAT(-1.687000), state_ML_57)
             | silent_transition(CONST_FLOAT(-0.566000), state_D_58)
             # h;

  state_IL_56 = transition(CONST_FLOAT(-1.442000), CHAR, state_IL_56; 56)
              | transition(CONST_FLOAT(-0.798000), CHAR, state_ML_57; 56)
              | transition(CONST_FLOAT(-4.142000), CHAR, state_D_58; 56)
              # h;

  // node [ MATL 12 ]
  state_ML_57 = transition(CONST_FLOAT(0.000000), CHAR, state_E_60; 57)
              # h;

  state_D_58 = silent_transition(CONST_FLOAT(0.000000), state_E_60)
             # h;

  state_IL_59 = transition(CONST_FLOAT(-1.823000), CHAR, state_IL_59; 59)
              | transition(CONST_FLOAT(-0.479000), CHAR, state_E_60; 59)
              # h;

  // node [ END 13 ]
  state_E_60 = nil(CONST_FLOAT(0.000000), EMPTY; 60)
             # h;

}

instance count = gra_cm(alg_count);

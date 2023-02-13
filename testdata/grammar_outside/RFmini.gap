
import "RFmini.hh"

signature sig_cm(alphabet, answer) {
  answer silent_transition(float, answer);
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
  float silent_transition(float tsc, float x) {
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
  state_S_0 = silent_transition(CONST_FLOAT(-9.486000), state_IL_1)
            | silent_transition(CONST_FLOAT(-19.955000), state_IR_2)
            | silent_transition(CONST_FLOAT(-0.002000), state_B_3)
            # h;

  state_IL_1 = left_transition(CONST_FLOAT(-0.610000), CHAR, state_IL_1; 1)
             | left_transition(CONST_FLOAT(-4.491000), CHAR, state_IR_2; 1)
             | left_transition(CONST_FLOAT(-1.736000), CHAR, state_B_3; 1)
             # h;

  state_IR_2 = right_transition(CONST_FLOAT(-1.823000), state_IR_2, CHAR; 2)
             | right_transition(CONST_FLOAT(-0.479000), state_B_3, CHAR; 2)
             # h;

  // node [ BIF 1 ]
  state_B_3 = bifurcation_transition(CONST_FLOAT(0.000000), CONST_FLOAT(0.000000), state_S_4, state_S_12; 3)
            # h;

  // node [ BEGL 2 ]
  state_S_4 = silent_transition(CONST_FLOAT(-0.006000), state_MP_5)
            | silent_transition(CONST_FLOAT(-9.761000), state_ML_6)
            | silent_transition(CONST_FLOAT(-9.168000), state_MR_7)
            | silent_transition(CONST_FLOAT(-9.808000), state_D_8)
            # h;

  // node [ MATP 3 ]
  state_MP_5 = pair_transition(CONST_FLOAT(-9.486000), CHAR, state_IL_9, CHAR; 5)
             | pair_transition(CONST_FLOAT(-0.002000), CHAR, state_E_11, CHAR; 5)
             # h;

  state_ML_6 = left_transition(CONST_FLOAT(-1.000000), CHAR, state_IL_9; 6)
             | left_transition(CONST_FLOAT(-1.000000), CHAR, state_E_11; 6)
             # h;

  state_MR_7 = right_transition(CONST_FLOAT(-1.000000), state_IL_9, CHAR; 7)
             | right_transition(CONST_FLOAT(-1.000000), state_E_11, CHAR; 7)
             # h;

  state_D_8 = silent_transition(CONST_FLOAT(-1.000000), state_IL_9)
            | silent_transition(CONST_FLOAT(-1.000000), state_E_11)
            # h;

  state_IL_9 = left_transition(CONST_FLOAT(-0.544000), CHAR, state_IL_9; 9)
             | left_transition(CONST_FLOAT(-1.670000), CHAR, state_E_11; 9)
             # h;

  state_IR_10 = right_transition(CONST_FLOAT(-1.823000), state_IR_10, CHAR; 10)
              | right_transition(CONST_FLOAT(-0.479000), state_E_11, CHAR; 10)
              # h;

  // node [ END 4 ]
  state_E_11 = nil(CONST_FLOAT(0.000000), EMPTY; 11)
             # h;

  // node [ BEGR 5 ]
  state_S_12 = silent_transition(CONST_FLOAT(-10.630000), state_IL_13)
             | silent_transition(CONST_FLOAT(-0.003000), state_MP_14)
             | silent_transition(CONST_FLOAT(-10.445000), state_ML_15)
             | silent_transition(CONST_FLOAT(-10.657000), state_MR_16)
             | silent_transition(CONST_FLOAT(-11.549000), state_D_17)
             # h;

  state_IL_13 = left_transition(CONST_FLOAT(-2.408000), CHAR, state_IL_13; 13)
              | left_transition(CONST_FLOAT(-0.496000), CHAR, state_MP_14; 13)
              | left_transition(CONST_FLOAT(-4.087000), CHAR, state_ML_15; 13)
              | left_transition(CONST_FLOAT(-5.920000), CHAR, state_MR_16; 13)
              | left_transition(CONST_FLOAT(-5.193000), CHAR, state_D_17; 13)
              # h;

  // node [ MATP 6 ]
  state_MP_14 = pair_transition(CONST_FLOAT(-9.486000), CHAR, state_IL_18, CHAR; 14)
              | pair_transition(CONST_FLOAT(-0.002000), CHAR, state_E_20, CHAR; 14)
              # h;

  state_ML_15 = left_transition(CONST_FLOAT(-1.000000), CHAR, state_IL_18; 15)
              | left_transition(CONST_FLOAT(-1.000000), CHAR, state_E_20; 15)
              # h;

  state_MR_16 = right_transition(CONST_FLOAT(-1.000000), state_IL_18, CHAR; 16)
              | right_transition(CONST_FLOAT(-1.000000), state_E_20, CHAR; 16)
              # h;

  state_D_17 = silent_transition(CONST_FLOAT(-1.000000), state_IL_18)
             | silent_transition(CONST_FLOAT(-1.000000), state_E_20)
             # h;

  state_IL_18 = left_transition(CONST_FLOAT(-0.544000), CHAR, state_IL_18; 18)
              | left_transition(CONST_FLOAT(-1.670000), CHAR, state_E_20; 18)
              # h;

  state_IR_19 = right_transition(CONST_FLOAT(-1.823000), state_IR_19, CHAR; 19)
              | right_transition(CONST_FLOAT(-0.479000), state_E_20, CHAR; 19)
              # h;

  // node [ END 7 ]
  state_E_20 = nil(CONST_FLOAT(0.000000), EMPTY; 20)
             # h;

}

instance count = gra_cm(alg_count);
instance enum = gra_cm(alg_enum);
instance enumcyk = gra_cm(alg_enum * alg_cyk);
instance cykenum = gra_cm(alg_cyk * alg_enum);
    
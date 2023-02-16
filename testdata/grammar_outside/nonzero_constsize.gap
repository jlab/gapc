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

grammar gra_cm uses sig_cm(axiom = state_B_3) {
  state_B_3 = bifurcation_transition(CONST_FLOAT(0.000000), CONST_FLOAT(0.000000), state_S_4, state_S_12; 3)
            # h;

  state_S_4 = silent_transition(CONST_FLOAT(-9.761000), state_ML_6; 4)
            | silent_transition(CONST_FLOAT(-9.808000), state_D_8; 4)
            # h;

  state_ML_6 = left_transition(CONST_FLOAT(-1.000000), CHAR, state_E_11; 6)
             # h;

  state_D_8 = silent_transition(CONST_FLOAT(-1.000000), state_E_11; 8)
            # h;

  state_E_11 = nil(CONST_FLOAT(0.000000), EMPTY; 11)
             # h;

        
  state_S_12 = silent_transition(CONST_FLOAT(-10.445000), state_ML_15; 12)
             | silent_transition(CONST_FLOAT(-11.549000), state_D_17; 12)
             # h;

  state_ML_15 = left_transition(CONST_FLOAT(-1.000000), CHAR, state_E_20; 15)
              # h;

  state_D_17 = silent_transition(CONST_FLOAT(-1.000000), state_E_20; 17)
             # h;

  state_E_20 = nil(CONST_FLOAT(0.000000), EMPTY; 20)
             # h;

}

instance count = gra_cm(alg_count);
instance enum = gra_cm(alg_enum);
    
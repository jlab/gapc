signature sig_cm(alphabet, answer) {
  answer silent_transition(answer);
  answer pair_transition(alphabet, answer, alphabet);
  answer nil(void);
  choice [answer] h([answer]);
}

algebra alg_enum auto enum;

algebra alg_count auto count;

grammar gra_wrong uses sig_cm(axiom = state_S_0) {
        tabulated { state_E_12 }

  state_S_0 = 
        silent_transition(state_MP_3) |
        silent_transition(state_E_12)
            # h;

  state_MP_3 = pair_transition(CHAR, state_E_12, CHAR)
             # h;

  state_E_12 = nil(EMPTY)
             # h;

}

grammar gra_ok uses sig_cm(axiom = state_S_0) {
        tabulated { state_MP_3 }

  state_S_0 = 
        silent_transition(state_MP_3) |
        silent_transition(state_E_12)
            # h;

  state_MP_3 = pair_transition(CHAR, state_E_12, CHAR)
             # h;

  state_E_12 = nil(EMPTY)
             # h;

}

// identical grammar, but different table designs lead to different results for the empty input "", namely
// [] for gra_wrong and 1 for gra_ok. Reason seems to be the statement
// if ((((t_0_i > 1) || (t_0_j < (t_0_n - 1))) || (t_0_j > (t_0_n - 1)))) {
//   return zero;
// }
// in the get() method of the state_E_12 table, namely the -1, which should be 0
      
instance enum = gra_wrong(alg_count);
instance ok = gra_ok(alg_count);

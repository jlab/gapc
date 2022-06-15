type Rope = extern

signature sig_tmhmm(alphabet, answer) {
  answer silent_step(answer);
  answer step(char, answer, answer);
  answer nil(void);
  answer trans(float, answer);
  answer only(float, alphabet);
  choice [answer] h([answer]);
}

algebra alg_enum auto enum;

algebra alg_count auto count;

algebra alg_viterbi implements sig_tmhmm(alphabet=char, answer=float) {
  float silent_step(float x) {
    return x;
  }
  float step(char label, float e, float t) {
    return e*t;
  }
  float nil(void) {
    return 1;
  }
  float trans(float prob, float x) {
    return prob * x;
  }
  float only(float prob, char emission) {
    return prob;
  }
  choice [float] h([float] candidates) {
    return list(maximum(candidates));
  }
}

algebra alg_viterbi_bit extends alg_viterbi {
  float step(char label, float e, float t) {
    return log(1.0/e) + t;
  }
  float nil(void) {
    return 0.0;
  }
  float trans(float prob, float x) {
    return log(1.0/prob) + x;
  }
  choice [float] h([float] candidates) {
    return list(minimum(candidates));
  }
}

algebra alg_label implements sig_tmhmm(alphabet=char, answer=Rope) {
  Rope silent_step(Rope x) {
    return x;
  }
  Rope step(char label, Rope e, Rope t) {
    Rope res;
    append(res, label);
    append(res, t);
    return res;
  }
  Rope nil(void) {
    Rope res;
    return res;
  }
  Rope trans(float prob, Rope x) {
    return x;
  }
  Rope only(float prob, char emission) {
    Rope res;
    return res;
  }
  choice [Rope] h([Rope] candidates) {
    return unique(candidates);
  }
}

grammar gra_tmhmm uses sig_tmhmm(axiom = state_begin) {
  state_begin = silent_step(transitions_begin) # h;
    transitions_begin =
      trans(CONST_FLOAT(0.549251), state_in10) |
      trans(CONST_FLOAT(0.207469), state_outglob10) |
      trans(CONST_FLOAT(0.24328), state_out10)
      # h;
  state_in10 = step(CONST_CHAR('i'), emissions_in10, { transitions_in10 | nil(EMPTY) }) # h;
    emissions_in10 =
      only(CONST_FLOAT(0.0713632), CHAR('A')) |
      only(CONST_FLOAT(0.0336358), CHAR('Y'))
      # h;
    transitions_in10 =
      trans(CONST_FLOAT(0.995851), state_in11) |
      trans(CONST_FLOAT(0.00111283), state_in29) |
      trans(CONST_FLOAT(0.00303586), state_ohelixi1)
      # h;
  state_in11 = step(CONST_CHAR('i'), emissions_in10, { transitions_in11 | nil(EMPTY) }) # h;
    transitions_in11 =
      trans(CONST_FLOAT(0.976066), nil(EMPTY)) |
      trans(CONST_FLOAT(0.0239339), nil(EMPTY)) |
      trans(CONST_FLOAT(1.08323e-09), state_in29)
      # h;
  state_in29 = step(CONST_CHAR('i'), emissions_in10, { transitions_in29 | nil(EMPTY) }) # h;
    transitions_in29 =
      trans(CONST_FLOAT(1.0), state_ohelixi1)
      # h;
  state_ohelixi1 = step(CONST_CHAR('M'), emissions_ohelixi1, transitions_ohelixi1) # h;
    emissions_ohelixi1 =
      only(CONST_FLOAT(0.0890049), CHAR('A')) |
      only(CONST_FLOAT(0.0661077), CHAR('Y'))
      # h;
    transitions_ohelixi1 =
      trans(CONST_FLOAT(1.0), nil(EMPTY))
      # h;
  state_outglob10 = step(CONST_CHAR('O'), emissions_outglob10, { transitions_outglob10 | nil(EMPTY) }) # h;
    emissions_outglob10 =
      only(CONST_FLOAT(0.0693743), CHAR('A')) |
      only(CONST_FLOAT(0.0275), CHAR('Y'))
      # h;
    transitions_outglob10 =
      trans(CONST_FLOAT(1.0), state_outglob11) |
      trans(CONST_FLOAT(0.0), state_outglob29) |
      trans(CONST_FLOAT(0.0), state_ihelixo1)
      # h;
  state_outglob11 = step(CONST_CHAR('O'), emissions_outglob10, { transitions_outglob11 | nil(EMPTY) }) # h;
    transitions_outglob11 =
      trans(CONST_FLOAT(1.0), nil(EMPTY)) |
      trans(CONST_FLOAT(0.0), nil(EMPTY)) |
      trans(CONST_FLOAT(0.0), state_outglob29)
      # h;
  state_outglob29 = step(CONST_CHAR('O'), emissions_outglob10, { transitions_outglob29 | nil(EMPTY) }) # h;
    transitions_outglob29 =
      trans(CONST_FLOAT(1.0), state_ihelixo1)
      # h;
  state_ihelixo1 = step(CONST_CHAR('M'), emissions_out10, transitions_ihelixo1) # h;
    transitions_ihelixo1 =
      trans(CONST_FLOAT(1.0), nil(EMPTY))
      # h;
  state_out10 = step(CONST_CHAR('o'), emissions_out10, { transitions_out10 | nil(EMPTY) }) # h;
    emissions_out10 =
      only(CONST_FLOAT(0.0690346), CHAR('A')) |
      only(CONST_FLOAT(0.0346461), CHAR('Y'))
      # h;
    transitions_out10 =
      trans(CONST_FLOAT(1.0), state_out11) |
      trans(CONST_FLOAT(3.54571e-14), state_out29) |
      trans(CONST_FLOAT(0.0), state_ihelixo1)
      # h;
  state_out11 = step(CONST_CHAR('o'), emissions_out10, { transitions_out11 | nil(EMPTY) }) # h;
    transitions_out11 =
      trans(CONST_FLOAT(0.910217), nil(EMPTY)) |
      trans(CONST_FLOAT(0.0495866), nil(EMPTY)) |
      trans(CONST_FLOAT(0.0401965), state_out29)
      # h;
  state_out29 = step(CONST_CHAR('o'), emissions_out10, { transitions_out29 | nil(EMPTY) }) # h;
    transitions_out29 =
      trans(CONST_FLOAT(1.0), state_ihelixo1)
      # h;
}

instance dummy = gra_tmhmm(alg_enum);

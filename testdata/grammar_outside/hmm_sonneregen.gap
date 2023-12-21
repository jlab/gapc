type Rope = extern

signature sig_weather(alphabet, answer) {
  answer transition_start_hoch(float, answer, answer);
  answer transition_start_tief(float, answer, answer);
  answer transition_hoch_tief(float, answer, answer);
  answer transition_hoch_hoch(float, answer, answer);
  answer transition_tief_tief(float, answer, answer);
  answer transition_tief_hoch(float, answer, answer);
  
  answer emission_hoch_sonne(float, alphabet);
  answer emission_hoch_regen(float, alphabet);
  answer emission_tief_sonne(float, alphabet);
  answer emission_tief_regen(float, alphabet);
  answer nil(void);
  
  choice [answer] h([answer]);
}

algebra alg_enum auto enum;
algebra alg_count auto count;
algebra alg_tikz auto tikz;

algebra alg_viterbi implements sig_weather(alphabet=char, answer=float) {
  float transition_start_hoch(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_start_tief(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_hoch_tief(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_hoch_hoch(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_tief_tief(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_tief_hoch(float transition, float emission, float x) {
    return transition * emission * x;
  }
  
  float emission_hoch_sonne(float emission, char a) {
    return emission;
  }
  float emission_hoch_regen(float emission, char a) {
    return emission;
  }
  float emission_tief_sonne(float emission, char a) {
    return emission;
  }
  float emission_tief_regen(float emission, char a) {
    return emission;
  }

  float nil(void) {
    return 1.0;
  }
  choice [float] h([float] candidates) {
    return list(maximum(candidates));
  }
}
algebra alg_fwd extends alg_viterbi {
  choice [float] h([float] candidates) {
    return list(sum(candidates));
  }
}

algebra alg_states implements sig_weather(alphabet=char, answer=Rope) {
  Rope transition_start_hoch(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, 'H');
    append(res, x);
    return res;
  }
  Rope transition_start_tief(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, 'T');
    append(res, x);
    return res;
  }
  Rope transition_hoch_tief(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, 'T');
    append(res, x);
    return res;
  }
  Rope transition_hoch_hoch(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, 'H');
    append(res, x);
    return res;
  }
  Rope transition_tief_tief(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, 'T');
    append(res, x);
    return res;
  }
  Rope transition_tief_hoch(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, 'H');
    append(res, x);
    return res;
  }
  
  Rope emission_hoch_sonne(float emission, char a) {
    Rope res;
    return res;
  }
  Rope emission_hoch_regen(float emission, char a) {
    Rope res;
    return res;
  }
  Rope emission_tief_sonne(float emission, char a) {
    Rope res;
    return res;
  }
  Rope emission_tief_regen(float emission, char a) {
    Rope res;
    return res;
  }

  Rope nil(void) {
    Rope res;
    return res;
  }
  choice [Rope] h([Rope] candidates) {
    return unique(candidates);
  }
}

algebra alg_mult implements sig_weather(alphabet=char, answer=Rope) {
  Rope transition_start_hoch(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, transition);
    append(res, " * ", 3);
    append(res, emission);
    append(res, " * ", 3);
    append(res, x);
    return res;
  }
  Rope transition_start_tief(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, transition);
    append(res, " * ", 3);
    append(res, emission);
    append(res, " * ", 3);
    append(res, x);
    return res;
  }
  Rope transition_hoch_tief(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, transition);
    append(res, " * ", 3);
    append(res, emission);
    append(res, " * ", 3);
    append(res, x);
    return res;
  }
  Rope transition_hoch_hoch(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, transition);
    append(res, " * ", 3);
    append(res, emission);
    append(res, " * ", 3);
    append(res, x);
    return res;
  }
  Rope transition_tief_tief(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, transition);
    append(res, " * ", 3);
    append(res, emission);
    append(res, " * ", 3);
    append(res, x);
    return res;
  }
  Rope transition_tief_hoch(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, transition);
    append(res, " * ", 3);
    append(res, emission);
    append(res, " * ", 3);
    append(res, x);
    return res;
  }
  
  Rope emission_hoch_sonne(float emission, char a) {
    Rope res;
    append(res, emission);
    return res;
  }
  Rope emission_hoch_regen(float emission, char a) {
    Rope res;
    append(res, emission);
    return res;
  }
  Rope emission_tief_sonne(float emission, char a) {
    Rope res;
    append(res, emission);
    return res;
  }
  Rope emission_tief_regen(float emission, char a) {
    Rope res;
    append(res, emission);
    return res;
  }

  Rope nil(void) {
    Rope res;
    append(res, 1.0);
    return res;
  }
  choice [Rope] h([Rope] candidates) {
    return unique(candidates);
  }
}


grammar gra_weather uses sig_weather(axiom=state_start) {
  state_start = transition_start_hoch(CONST_FLOAT(0.5), emit_hoch, state_hoch)
              | transition_start_tief(CONST_FLOAT(0.5), emit_tief, state_tief)
              | nil(EMPTY)
              # h;

  state_hoch  = transition_hoch_tief(CONST_FLOAT(0.3), emit_tief, state_tief)
              | transition_hoch_hoch(CONST_FLOAT(0.7), emit_hoch, state_hoch)
              | nil(EMPTY)
              # h;

  state_tief  = transition_tief_tief(CONST_FLOAT(0.6), emit_tief, state_tief)
              | transition_tief_hoch(CONST_FLOAT(0.4), emit_hoch, state_hoch)
              | nil(EMPTY)
              # h;


  emit_hoch = emission_hoch_sonne(CONST_FLOAT(0.8), CHAR('S'))
            | emission_hoch_regen(CONST_FLOAT(0.2), CHAR('R'))
            # h;

  emit_tief = emission_tief_sonne(CONST_FLOAT(0.1), CHAR('S'))
            | emission_tief_regen(CONST_FLOAT(0.9), CHAR('R'))
            # h;
}

instance enum = gra_weather(alg_enum);
instance viterbistatesmult = gra_weather(alg_viterbi * alg_states * alg_mult);
instance fwd = gra_weather(alg_fwd);
instance multviterbistates = gra_weather(alg_mult * alg_viterbi * alg_states);
instance count = gra_weather(alg_count);
instance tikz = gra_weather(alg_tikz);
instance tikzbtrace = gra_weather(alg_viterbi * alg_tikz);

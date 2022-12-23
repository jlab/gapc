type Rope = extern

signature sig_weather(alphabet, answer) {
  answer transition_start_hoch(float, answer, answer);
  answer transition_start_tief(float, answer, answer);
  answer transition_start_ende(float, answer);
  answer transition_hoch_tief(float, answer, answer);
  answer transition_hoch_hoch(float, answer, answer);
  answer transition_hoch_ende(float, answer);
  answer transition_tief_tief(float, answer, answer);
  answer transition_tief_hoch(float, answer, answer);
  answer transition_tief_ende(float, answer);
  
  answer emission_hoch_sonne(float, alphabet);
  answer emission_hoch_regen(float, alphabet);
  answer emission_tief_sonne(float, alphabet);
  answer emission_tief_regen(float, alphabet);
  answer nil(void);
  
  choice [answer] h([answer]);
}

algebra alg_enum auto enum;
algebra alg_count auto count;

algebra alg_viterbi implements sig_weather(alphabet=char, answer=float) {
  float transition_start_hoch(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_start_tief(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_start_ende(float transition, float x) {
    return transition * x;
  }
  float transition_hoch_tief(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_hoch_hoch(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_hoch_ende(float transition, float x) {
    return transition * x;
  }
  float transition_tief_tief(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_tief_hoch(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_tief_ende(float transition, float x) {
    return transition * x;
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

algebra alg_experiment implements sig_weather(alphabet=char, answer=float) {
  float transition_start_hoch(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_start_tief(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_start_ende(float transition, float x) {
    return transition * x;
  }
  float transition_hoch_tief(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_hoch_hoch(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_hoch_ende(float transition, float x) {
    return transition * x;
  }
  float transition_tief_tief(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_tief_hoch(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_tief_ende(float transition, float x) {
    return transition * x;
  }
  
  float emission_hoch_sonne(float emission, char a) {
    return 1.0;
  }
  float emission_hoch_regen(float emission, char a) {
    return 1.0;
  }
  float emission_tief_sonne(float emission, char a) {
    return 1.0;
  }
  float emission_tief_regen(float emission, char a) {
    return 1.0;
  }
  float nil(void) {
    return 1.0;
  }

  choice [float] h([float] candidates) {
    return list(sum(candidates));
  }
}

algebra alg_fwd extends alg_viterbi {
  float normalize_derivative(float q, float pfunc) {
    return q / pfunc;
  }
  choice [float] h([float] candidates) {
    return list(sum(candidates));
  }
}

algebra alg_states implements sig_weather(alphabet=char, answer=Rope) {
  Rope transition_start_hoch(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, '^');
    append(res, 'H');
    append(res, x);
    return res;
  }
  Rope transition_start_tief(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, '^');
    append(res, 'T');
    append(res, x);
    return res;
  }
  Rope transition_start_ende(float transition, Rope x) {
    Rope res;
    append(res, '^');
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
  Rope transition_hoch_ende(float transition, Rope x) {
    Rope res;
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
  Rope transition_tief_ende(float transition, Rope x) {
    Rope res;
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
    append(res, '$');
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
  Rope transition_start_ende(float transition, Rope x) {
    Rope res;
    append(res, transition);
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
  Rope transition_hoch_ende(float transition, Rope x) {
    Rope res;
    append(res, transition);
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
  Rope transition_tief_ende(float transition, Rope x) {
    Rope res;
    append(res, transition);
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


grammar gra_weather uses sig_weather(axiom=start) {
  start = transition_start_hoch(CONST_FLOAT(0.49), hoch_emission, hoch)
        | transition_start_tief(CONST_FLOAT(0.49), tief_emission, tief)
        | transition_start_ende(CONST_FLOAT(0.02), ende)
        # h;

  hoch  = transition_hoch_tief(CONST_FLOAT(0.29), tief_emission, tief)
        | transition_hoch_hoch(CONST_FLOAT(0.69), hoch_emission, hoch)
        | transition_hoch_ende(CONST_FLOAT(0.02), ende)
        # h;

  tief  = transition_tief_tief(CONST_FLOAT(0.59), tief_emission, tief)
        | transition_tief_hoch(CONST_FLOAT(0.39), hoch_emission, hoch)
        | transition_tief_ende(CONST_FLOAT(0.02), ende)
        # h;
        
  ende = nil(EMPTY)
       # h; 

  hoch_emission = emission_hoch_sonne(CONST_FLOAT(0.8), CHAR('S'))
                | emission_hoch_regen(CONST_FLOAT(0.2), CHAR('R'))
                # h;

  tief_emission = emission_tief_sonne(CONST_FLOAT(0.1), CHAR('S'))
                | emission_tief_regen(CONST_FLOAT(0.9), CHAR('R'))
                # h;
}

instance enum = gra_weather(alg_enum);
instance viterbistatesmult = gra_weather(alg_viterbi * alg_states * alg_mult);
instance fwd = gra_weather(alg_fwd);
instance multviterbistates = gra_weather(alg_mult * alg_viterbi * alg_states);

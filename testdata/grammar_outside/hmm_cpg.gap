type Rope = extern

signature sig_cpg(alphabet, answer) {
  answer transition_start_rich(float, answer, answer);
  answer transition_start_poor(float, answer, answer);
  answer transition_rich_rich(float, answer, answer);
  answer transition_rich_poor(float, answer, answer);
  answer transition_poor_rich(float, answer, answer);
  answer transition_poor_poor(float, answer, answer);
  
  answer emission_rich_A(float, alphabet);
  answer emission_rich_C(float, alphabet);
  answer emission_rich_G(float, alphabet);
  answer emission_rich_T(float, alphabet);
  answer emission_poor_A(float, alphabet);
  answer emission_poor_C(float, alphabet);
  answer emission_poor_G(float, alphabet);
  answer emission_poor_T(float, alphabet);
  answer nil(void);
  
  choice [answer] h([answer]);
}

algebra alg_enum auto enum;
algebra alg_count auto count;

algebra alg_viterbi implements sig_cpg(alphabet=char, answer=float) {
  float transition_start_rich(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_start_poor(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_rich_poor(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_rich_rich(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_poor_poor(float transition, float emission, float x) {
    return transition * emission * x;
  }
  float transition_poor_rich(float transition, float emission, float x) {
    return transition * emission * x;
  }
  
  float emission_rich_A(float emission, char a) {
    return emission;
  }
  float emission_rich_C(float emission, char a) {
    return emission;
  }
  float emission_rich_G(float emission, char a) {
    return emission;
  }
  float emission_rich_T(float emission, char a) {
    return emission;
  }
  float emission_poor_A(float emission, char a) {
    return emission;
  }
  float emission_poor_C(float emission, char a) {
    return emission;
  }
  float emission_poor_G(float emission, char a) {
    return emission;
  }
  float emission_poor_T(float emission, char a) {
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

algebra alg_states implements sig_cpg(alphabet=char, answer=Rope) {
  Rope transition_start_rich(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, 'H');
    append(res, x);
    return res;
  }
  Rope transition_start_poor(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, 'L');
    append(res, x);
    return res;
  }
  Rope transition_rich_poor(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, 'L');
    append(res, x);
    return res;
  }
  Rope transition_rich_rich(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, 'H');
    append(res, x);
    return res;
  }
  Rope transition_poor_poor(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, 'L');
    append(res, x);
    return res;
  }
  Rope transition_poor_rich(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, 'H');
    append(res, x);
    return res;
  }
  
  Rope emission_rich_A(float emission, char a) {
    Rope res;
    return res;
  }
  Rope emission_rich_C(float emission, char a) {
    Rope res;
    return res;
  }
  Rope emission_rich_G(float emission, char a) {
    Rope res;
    return res;
  }
  Rope emission_rich_T(float emission, char a) {
    Rope res;
    return res;
  }
  Rope emission_poor_A(float emission, char a) {
    Rope res;
    return res;
  }
  Rope emission_poor_C(float emission, char a) {
    Rope res;
    return res;
  }
  Rope emission_poor_G(float emission, char a) {
    Rope res;
    return res;
  }
  Rope emission_poor_T(float emission, char a) {
    Rope res;
    return res;
  }

  Rope nil(void) {
    Rope res;
    return res;
  }
  choice [Rope] h([Rope] candidates) {
    return candidates;
  }
}

algebra alg_mult implements sig_cpg(alphabet=char, answer=Rope) {
  Rope transition_start_rich(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, transition);
    append(res, " * ", 3);
    append(res, emission);
    append(res, " * ", 3);
    append(res, x);
    return res;
  }
  Rope transition_start_poor(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, transition);
    append(res, " * ", 3);
    append(res, emission);
    append(res, " * ", 3);
    append(res, x);
    return res;
  }
  Rope transition_rich_poor(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, transition);
    append(res, " * ", 3);
    append(res, emission);
    append(res, " * ", 3);
    append(res, x);
    return res;
  }
  Rope transition_rich_rich(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, transition);
    append(res, " * ", 3);
    append(res, emission);
    append(res, " * ", 3);
    append(res, x);
    return res;
  }
  Rope transition_poor_poor(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, transition);
    append(res, " * ", 3);
    append(res, emission);
    append(res, " * ", 3);
    append(res, x);
    return res;
  }
  Rope transition_poor_rich(float transition, Rope emission, Rope x) {
    Rope res;
    append(res, transition);
    append(res, " * ", 3);
    append(res, emission);
    append(res, " * ", 3);
    append(res, x);
    return res;
  }
  
  Rope emission_rich_A(float emission, char a) {
    Rope res;
    append(res, emission);
    return res;
  }
  Rope emission_rich_C(float emission, char a) {
    Rope res;
    append(res, emission);
    return res;
  }
  Rope emission_rich_G(float emission, char a) {
    Rope res;
    append(res, emission);
    return res;
  }
  Rope emission_rich_T(float emission, char a) {
    Rope res;
    append(res, emission);
    return res;
  }
  Rope emission_poor_A(float emission, char a) {
    Rope res;
    append(res, emission);
    return res;
  }
  Rope emission_poor_C(float emission, char a) {
    Rope res;
    append(res, emission);
    return res;
  }
  Rope emission_poor_G(float emission, char a) {
    Rope res;
    append(res, emission);
    return res;
  }
  Rope emission_poor_T(float emission, char a) {
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


grammar gra_cpg uses sig_cpg(axiom=start) {
  start = transition_start_rich(CONST_FLOAT(0.5), rich_emission, rich)
        | transition_start_poor(CONST_FLOAT(0.5), poor_emission, poor)
        # h;

  rich  = transition_rich_rich(CONST_FLOAT(0.63), rich_emission, rich)
        | transition_rich_poor(CONST_FLOAT(0.37), poor_emission, poor)
	| nil(EMPTY)
        # h;
	
  poor  = transition_poor_poor(CONST_FLOAT(0.63), poor_emission, poor)
        | transition_poor_rich(CONST_FLOAT(0.37), rich_emission, rich)
	| nil(EMPTY)
        # h;
	
  rich_emission = emission_rich_A(CONST_FLOAT(0.13), CHAR('A'))
		| emission_rich_C(CONST_FLOAT(0.37), CHAR('C'))
		| emission_rich_G(CONST_FLOAT(0.37), CHAR('G'))
		| emission_rich_T(CONST_FLOAT(0.13), CHAR('T'))
		# h;

  poor_emission = emission_poor_A(CONST_FLOAT(0.37), CHAR('A'))
		| emission_poor_C(CONST_FLOAT(0.13), CHAR('C'))
		| emission_poor_G(CONST_FLOAT(0.13), CHAR('G'))
		| emission_poor_T(CONST_FLOAT(0.37), CHAR('T'))
		# h;
}

instance enum = gra_cpg(alg_enum);
instance viterbistatesmult = gra_cpg(alg_viterbi * alg_states * alg_mult);
instance fwd = gra_cpg(alg_fwd);
instance multviterbistates = gra_cpg(alg_mult * alg_viterbi * alg_states);

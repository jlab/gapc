import "ext_hmm.hh"
type Rope = extern

signature sig_tmhmm(alphabet, answer) {
  answer silent_transition(float, answer);
  answer transition(char, float, answer, answer);
  answer nil(void);
  answer emission(float, alphabet);
  choice [answer] h([answer]);
}

algebra alg_enum auto enum;

algebra alg_count auto count;

algebra alg_viterbi implements sig_tmhmm(alphabet=char, answer=float) {
  float silent_transition(float prob, float t) {
    return prob * t;
  }
  float transition(char label, float prob, float e, float t) {
    return prob * e * t;
  }
  float nil(void) {
    return 1.0;
  }
  float emission(float prob, char emission) {
    return prob;
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

algebra alg_fwd_scaled extends alg_viterbi {
  float emission(float prob, char emission) {
    /* 43.38 is a scaling factor against numeric instability,
     * as candidate probabilities tend to become very small.
     * The value is 1 / median of all emission probabilities
     * in the TMHMM2 model; but in principle can be any value > 1.
     */
    return 22.56 * prob;
  }
  float normalize_derivative(float q, float pfunc) {
    return q / pfunc;
  }
  choice [float] h([float] candidates) {
    return list(sum(candidates));
  }
}

algebra alg_viterbi_bit extends alg_viterbi {
  float silent_transition(float prob, float t) {
    return log(1.0/prob) + t;
  }
  float transition(char label, float prob, float e, float t) {
    return log(1.0/prob) + e + t;
  }
  float nil(void) {
    return 0.0;
  }
  float emission(float prob, char emission) {
    return log(1.0/prob);
  }
  choice [float] h([float] candidates) {
    return list(minimum(candidates));
  }
}

algebra alg_fwd_bit extends alg_viterbi_bit {
  float normalize_derivative(float q, float pfunc) {
    return exp(pfunc - q);
  }
  choice [float] h([float] candidates) {
    return list(negexpsum(candidates));
  }
}

algebra alg_label implements sig_tmhmm(alphabet=char, answer=Rope) {
  Rope silent_transition(float prob, Rope x) {
    return x;
  }
  Rope transition(char label, float prob, Rope e, Rope t) {
    Rope res;
    append(res, label);
    append(res, t);
    return res;
  }
  Rope nil(void) {
    Rope res;
    return res;
  }
  Rope emission(float prob, char emission) {
    Rope res;
    return res;
  }
  choice [Rope] h([Rope] candidates) {
    return unique(candidates);
  }
}

grammar gra_tmhmm uses sig_tmhmm(axiom = state_begin) {
  state_begin =
    silent_transition(CONST_FLOAT(0.549251), state_in10) |
    silent_transition(CONST_FLOAT(0.207469), state_outglob10) |
    silent_transition(CONST_FLOAT(0.24328), state_out10) |
    state_end
    # h;

  state_in10 =
    transition(CONST_CHAR('i'), CONST_FLOAT(0.995851), emit_in10, state_in11) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.00111283), emit_in10, state_in29) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.00303586), emit_in10, state_ohelixi1) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;
  emit_in10 =
    emission(CONST_FLOAT(0.0713632), CHAR('A')) |
    emission(CONST_FLOAT(0.0188491), CHAR('C')) |
    emission(CONST_FLOAT(0.043014), CHAR('D')) |
    emission(CONST_FLOAT(0.0471663), CHAR('E')) |
    emission(CONST_FLOAT(0.0298789), CHAR('F')) |
    emission(CONST_FLOAT(0.0564853), CHAR('G')) |
    emission(CONST_FLOAT(0.0233906), CHAR('H')) |
    emission(CONST_FLOAT(0.038904), CHAR('I')) |
    emission(CONST_FLOAT(0.0894525), CHAR('K')) |
    emission(CONST_FLOAT(0.0551519), CHAR('L')) |
    emission(CONST_FLOAT(0.0427067), CHAR('M')) |
    emission(CONST_FLOAT(0.0544149), CHAR('N')) |
    emission(CONST_FLOAT(0.0370019), CHAR('P')) |
    emission(CONST_FLOAT(0.0524006), CHAR('Q')) |
    emission(CONST_FLOAT(0.114758), CHAR('R')) |
    emission(CONST_FLOAT(0.0661936), CHAR('S')) |
    emission(CONST_FLOAT(0.0689907), CHAR('T')) |
    emission(CONST_FLOAT(0.0416332), CHAR('V')) |
    emission(CONST_FLOAT(0.0146085), CHAR('W')) |
    emission(CONST_FLOAT(0.0336358), CHAR('Y'))
    # h;

  state_in11 =
    transition(CONST_CHAR('i'), CONST_FLOAT(0.976066), emit_in10, state_in12) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.0239339), emit_in10, state_in28) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.08323e-09), emit_in10, state_in29) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in12 =
    transition(CONST_CHAR('i'), CONST_FLOAT(0.895077), emit_in10, state_in13) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.104922), emit_in10, state_in27) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.76891e-06), emit_in10, state_in28) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in13 =
    transition(CONST_CHAR('i'), CONST_FLOAT(0.979527), emit_in10, state_in14) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.0204673), emit_in10, state_in26) |
    transition(CONST_CHAR('i'), CONST_FLOAT(5.81809e-06), emit_in10, state_in27) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in14 =
    transition(CONST_CHAR('i'), CONST_FLOAT(0.993341), emit_in10, state_in15) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.00660812), emit_in10, state_in25) |
    transition(CONST_CHAR('i'), CONST_FLOAT(5.08664e-05), emit_in10, state_in26) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in15 =
    transition(CONST_CHAR('i'), CONST_FLOAT(0.738969), emit_in10, state_in16) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.159734), emit_in10, state_in24) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.101297), emit_in10, state_in25) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in16 =
    transition(CONST_CHAR('i'), CONST_FLOAT(0.999938), emit_in10, state_in17) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.4427e-06), emit_in10, state_in23) |
    transition(CONST_CHAR('i'), CONST_FLOAT(6.0424e-05), emit_in10, state_in24) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in17 =
    transition(CONST_CHAR('i'), CONST_FLOAT(0.973203), emit_in10, state_in18) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.0168132), emit_in10, state_in22) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.00998417), emit_in10, state_in23) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in18 =
    transition(CONST_CHAR('i'), CONST_FLOAT(0.977498), emit_in10, state_in19) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.0217216), emit_in10, state_in21) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.000780768), emit_in10, state_in22) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in19 =
    transition(CONST_CHAR('i'), CONST_FLOAT(0.16223), emit_in10, state_in20) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.10126), emit_in10, state_in21) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.73651), emit_in10, state_inglob1) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in20 =
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_in21) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in21 =
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_in22) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in22 =
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_in23) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in23 =
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_in24) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in24 =
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_in25) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in25 =
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_in26) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in26 =
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_in27) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in27 =
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_in28) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in28 =
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_in29) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_in29 =
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_ohelixi1) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_in10, state_end)
    # h;

  state_inglob1 =
    transition(CONST_CHAR('i'), CONST_FLOAT(0.0132918), emit_inglob1, state_in20) |
    transition(CONST_CHAR('i'), CONST_FLOAT(0.986708), emit_inglob1, state_inglob1) |
    transition(CONST_CHAR('i'), CONST_FLOAT(1.0), emit_inglob1, state_end)
    # h;
  emit_inglob1 =
    emission(CONST_FLOAT(0.0773341), CHAR('A')) |
    emission(CONST_FLOAT(0.0212026), CHAR('C')) |
    emission(CONST_FLOAT(0.0556231), CHAR('D')) |
    emission(CONST_FLOAT(0.0789783), CHAR('E')) |
    emission(CONST_FLOAT(0.0291466), CHAR('F')) |
    emission(CONST_FLOAT(0.0821038), CHAR('G')) |
    emission(CONST_FLOAT(0.02529), CHAR('H')) |
    emission(CONST_FLOAT(0.0392883), CHAR('I')) |
    emission(CONST_FLOAT(0.0466567), CHAR('K')) |
    emission(CONST_FLOAT(0.0718204), CHAR('L')) |
    emission(CONST_FLOAT(0.0191835), CHAR('M')) |
    emission(CONST_FLOAT(0.0490524), CHAR('N')) |
    emission(CONST_FLOAT(0.0671432), CHAR('P')) |
    emission(CONST_FLOAT(0.0472671), CHAR('Q')) |
    emission(CONST_FLOAT(0.0492684), CHAR('R')) |
    emission(CONST_FLOAT(0.0852997), CHAR('S')) |
    emission(CONST_FLOAT(0.0610192), CHAR('T')) |
    emission(CONST_FLOAT(0.0528717), CHAR('V')) |
    emission(CONST_FLOAT(0.0166592), CHAR('W')) |
    emission(CONST_FLOAT(0.0247916), CHAR('Y'))
    # h;

  state_outglob10 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob11) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;
  emit_outglob10 =
    emission(CONST_FLOAT(0.0693743), CHAR('A')) |
    emission(CONST_FLOAT(0.0149605), CHAR('C')) |
    emission(CONST_FLOAT(0.0406956), CHAR('D')) |
    emission(CONST_FLOAT(0.0538397), CHAR('E')) |
    emission(CONST_FLOAT(0.0531778), CHAR('F')) |
    emission(CONST_FLOAT(0.0792746), CHAR('G')) |
    emission(CONST_FLOAT(0.0221055), CHAR('H')) |
    emission(CONST_FLOAT(0.0440866), CHAR('I')) |
    emission(CONST_FLOAT(0.0565779), CHAR('K')) |
    emission(CONST_FLOAT(0.0988165), CHAR('L')) |
    emission(CONST_FLOAT(0.0432829), CHAR('M')) |
    emission(CONST_FLOAT(0.0414346), CHAR('N')) |
    emission(CONST_FLOAT(0.0615562), CHAR('P')) |
    emission(CONST_FLOAT(0.0412212), CHAR('Q')) |
    emission(CONST_FLOAT(0.0677628), CHAR('R')) |
    emission(CONST_FLOAT(0.0732544), CHAR('S')) |
    emission(CONST_FLOAT(0.0524824), CHAR('T')) |
    emission(CONST_FLOAT(0.0445653), CHAR('V')) |
    emission(CONST_FLOAT(0.0140309), CHAR('W')) |
    emission(CONST_FLOAT(0.0275), CHAR('Y'))
    # h;

  state_outglob11 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob12) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob12 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob13) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob13 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob14) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob14 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob15) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob15 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob16) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob16 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob17) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob17 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob18) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob18 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob19) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob19 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglobLong) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglobLong =
    transition(CONST_CHAR('O'), CONST_FLOAT(0.999093), emit_inglob1, state_outglobLong) |
    transition(CONST_CHAR('O'), CONST_FLOAT(0.000906913), emit_inglob1, state_outglob20) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_inglob1, state_end)
    # h;

  state_outglob20 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob21) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob21 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob22) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob22 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob23) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob23 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob24) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob24 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob25) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob25 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob26) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob26 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob27) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob27 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob28) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob28 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_outglob29) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_outglob29 =
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_ihelixo1) |
    transition(CONST_CHAR('O'), CONST_FLOAT(1.0), emit_outglob10, state_end)
    # h;

  state_out10 =
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_out11) |
    transition(CONST_CHAR('o'), CONST_FLOAT(3.54571e-14), emit_out10, state_out29) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;
  emit_out10 =
    emission(CONST_FLOAT(0.0690346), CHAR('A')) |
    emission(CONST_FLOAT(0.0129673), CHAR('C')) |
    emission(CONST_FLOAT(0.0627756), CHAR('D')) |
    emission(CONST_FLOAT(0.0541851), CHAR('E')) |
    emission(CONST_FLOAT(0.0425402), CHAR('F')) |
    emission(CONST_FLOAT(0.0835626), CHAR('G')) |
    emission(CONST_FLOAT(0.0271581), CHAR('H')) |
    emission(CONST_FLOAT(0.0292546), CHAR('I')) |
    emission(CONST_FLOAT(0.0530401), CHAR('K')) |
    emission(CONST_FLOAT(0.0822093), CHAR('L')) |
    emission(CONST_FLOAT(0.0334625), CHAR('M')) |
    emission(CONST_FLOAT(0.0506017), CHAR('N')) |
    emission(CONST_FLOAT(0.0693889), CHAR('P')) |
    emission(CONST_FLOAT(0.0389539), CHAR('Q')) |
    emission(CONST_FLOAT(0.0432109), CHAR('R')) |
    emission(CONST_FLOAT(0.0863749), CHAR('S')) |
    emission(CONST_FLOAT(0.0587278), CHAR('T')) |
    emission(CONST_FLOAT(0.0480586), CHAR('V')) |
    emission(CONST_FLOAT(0.0198473), CHAR('W')) |
    emission(CONST_FLOAT(0.0346461), CHAR('Y'))
    # h;

  state_out11 =
    transition(CONST_CHAR('o'), CONST_FLOAT(0.910217), emit_out10, state_out12) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.0495866), emit_out10, state_out28) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.0401965), emit_out10, state_out29) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out12 =
    transition(CONST_CHAR('o'), CONST_FLOAT(0.984498), emit_out10, state_out13) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.00440955), emit_out10, state_out27) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.0110921), emit_out10, state_out28) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out13 =
    transition(CONST_CHAR('o'), CONST_FLOAT(0.997286), emit_out10, state_out14) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.00258189), emit_out10, state_out26) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.000132519), emit_out10, state_out27) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out14 =
    transition(CONST_CHAR('o'), CONST_FLOAT(0.812906), emit_out10, state_out15) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.0130127), emit_out10, state_out25) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.174082), emit_out10, state_out26) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out15 =
    transition(CONST_CHAR('o'), CONST_FLOAT(0.951951), emit_out10, state_out16) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.0400992), emit_out10, state_out24) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.00795001), emit_out10, state_out25) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out16 =
    transition(CONST_CHAR('o'), CONST_FLOAT(0.660205), emit_out10, state_out17) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.33979), emit_out10, state_out23) |
    transition(CONST_CHAR('o'), CONST_FLOAT(4.76867e-06), emit_out10, state_out24) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out17 =
    transition(CONST_CHAR('o'), CONST_FLOAT(0.992733), emit_out10, state_out18) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.0072618), emit_out10, state_out22) |
    transition(CONST_CHAR('o'), CONST_FLOAT(5.34599e-06), emit_out10, state_out23) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out18 =
    transition(CONST_CHAR('o'), CONST_FLOAT(0.971165), emit_out10, state_out19) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.0119931), emit_out10, state_out21) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.0168416), emit_out10, state_out22) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out19 =
    transition(CONST_CHAR('o'), CONST_FLOAT(0.770716), emit_out10, state_outglobShort) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.00133523), emit_out10, state_out20) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.227948), emit_out10, state_out21) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_outglobShort =
    transition(CONST_CHAR('o'), CONST_FLOAT(0.960334), emit_inglob1, state_outglobShort) |
    transition(CONST_CHAR('o'), CONST_FLOAT(0.0396664), emit_inglob1, state_out20) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_inglob1, state_end)
    # h;

  state_out20 =
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_out21) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out21 =
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_out22) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out22 =
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_out23) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out23 =
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_out24) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out24 =
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_out25) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out25 =
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_out26) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out26 =
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_out27) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out27 =
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_out28) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out28 =
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_out29) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_out29 =
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_ihelixo1) |
    transition(CONST_CHAR('o'), CONST_FLOAT(1.0), emit_out10, state_end)
    # h;

  state_ohelixi1 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixi1, state_ohelixi2)
    # h;
  emit_ohelixi1 =
    emission(CONST_FLOAT(0.0890049), CHAR('A')) |
    emission(CONST_FLOAT(0.0170933), CHAR('C')) |
    emission(CONST_FLOAT(0.00159548), CHAR('D')) |
    emission(CONST_FLOAT(0.00489647), CHAR('E')) |
    emission(CONST_FLOAT(0.108081), CHAR('F')) |
    emission(CONST_FLOAT(0.0692068), CHAR('G')) |
    emission(CONST_FLOAT(0.00923517), CHAR('H')) |
    emission(CONST_FLOAT(0.113471), CHAR('I')) |
    emission(CONST_FLOAT(0.00466208), CHAR('K')) |
    emission(CONST_FLOAT(0.19417), CHAR('L')) |
    emission(CONST_FLOAT(0.0239901), CHAR('M')) |
    emission(CONST_FLOAT(0.0233656), CHAR('N')) |
    emission(CONST_FLOAT(0.0145168), CHAR('P')) |
    emission(CONST_FLOAT(0.00487025), CHAR('Q')) |
    emission(CONST_FLOAT(0.0127643), CHAR('R')) |
    emission(CONST_FLOAT(0.0455139), CHAR('S')) |
    emission(CONST_FLOAT(0.0292949), CHAR('T')) |
    emission(CONST_FLOAT(0.128208), CHAR('V')) |
    emission(CONST_FLOAT(0.0399529), CHAR('W')) |
    emission(CONST_FLOAT(0.0661077), CHAR('Y'))
    # h;

  state_ohelixi2 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixi1, state_ohelixi3)
    # h;

  state_ohelixi3 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixi1, state_ohelixi4)
    # h;

  state_ohelixi4 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixi1, state_ohelixi5)
    # h;

  state_ohelixi5 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixi1, state_ohelixi6)
    # h;

  state_ohelixi6 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelixi7)
    # h;

  state_ohelixi7 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelixm)
    # h;

  state_ohelixm =
    transition(CONST_CHAR('M'), CONST_FLOAT(0.000534023), emit_ohelixm, state_ohelix2) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.000567583), emit_ohelixm, state_ohelix3) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00064457), emit_ohelixm, state_ohelix4) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00159544), emit_ohelixm, state_ohelix5) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.000669433), emit_ohelixm, state_ohelix6) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00161008), emit_ohelixm, state_ohelix7) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.000762887), emit_ohelixm, state_ohelix8) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.000789084), emit_ohelixm, state_ohelix9) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.000868204), emit_ohelixm, state_ohelix10) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00519996), emit_ohelixm, state_ohelix11) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00774891), emit_ohelixm, state_ohelix12) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.0108947), emit_ohelixm, state_ohelix13) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.697722), emit_ohelixm, state_ohelix14) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.0444777), emit_ohelixm, state_ohelix15) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.0328707), emit_ohelixm, state_ohelix16) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.111827), emit_ohelixm, state_ohelix17) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.0324248), emit_ohelixm, state_ohelix18) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.0448694), emit_ohelixm, state_ohelix19) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00212234), emit_ohelixm, state_ohelix20) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00107586), emit_ohelixm, state_ohelix21) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00072618), emit_ohelixm, state_ohelixo1)
    # h;
  emit_ohelixm =
    emission(CONST_FLOAT(0.110896), CHAR('A')) |
    emission(CONST_FLOAT(0.0254931), CHAR('C')) |
    emission(CONST_FLOAT(0.00235724), CHAR('D')) |
    emission(CONST_FLOAT(0.00383965), CHAR('E')) |
    emission(CONST_FLOAT(0.0942003), CHAR('F')) |
    emission(CONST_FLOAT(0.0818095), CHAR('G')) |
    emission(CONST_FLOAT(0.00408389), CHAR('H')) |
    emission(CONST_FLOAT(0.144377), CHAR('I')) |
    emission(CONST_FLOAT(0.00236432), CHAR('K')) |
    emission(CONST_FLOAT(0.182902), CHAR('L')) |
    emission(CONST_FLOAT(0.0385107), CHAR('M')) |
    emission(CONST_FLOAT(0.00978815), CHAR('N')) |
    emission(CONST_FLOAT(0.0201094), CHAR('P')) |
    emission(CONST_FLOAT(0.00437833), CHAR('Q')) |
    emission(CONST_FLOAT(0.00115335), CHAR('R')) |
    emission(CONST_FLOAT(0.0421756), CHAR('S')) |
    emission(CONST_FLOAT(0.0514071), CHAR('T')) |
    emission(CONST_FLOAT(0.132167), CHAR('V')) |
    emission(CONST_FLOAT(0.0158643), CHAR('W')) |
    emission(CONST_FLOAT(0.0321232), CHAR('Y'))
    # h;

  state_ohelix2 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix3)
    # h;

  state_ohelix3 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix4)
    # h;

  state_ohelix4 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix5)
    # h;

  state_ohelix5 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix6)
    # h;

  state_ohelix6 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix7)
    # h;

  state_ohelix7 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix8)
    # h;

  state_ohelix8 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix9)
    # h;

  state_ohelix9 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix10)
    # h;

  state_ohelix10 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix11)
    # h;

  state_ohelix11 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix12)
    # h;

  state_ohelix12 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix13)
    # h;

  state_ohelix13 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix14)
    # h;

  state_ohelix14 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix15)
    # h;

  state_ohelix15 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix16)
    # h;

  state_ohelix16 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix17)
    # h;

  state_ohelix17 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix18)
    # h;

  state_ohelix18 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix19)
    # h;

  state_ohelix19 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix20)
    # h;

  state_ohelix20 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelix21)
    # h;

  state_ohelix21 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelixo1)
    # h;

  state_ohelixo1 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelixo2)
    # h;

  state_ohelixo2 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ohelixo3)
    # h;

  state_ohelixo3 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixo7, state_ohelixo4)
    # h;

  state_ohelixo4 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixo7, state_ohelixo5)
    # h;

  state_ohelixo5 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixo7, state_ohelixo6)
    # h;

  state_ohelixo6 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixo7, state_ohelixo7)
    # h;

  state_ohelixo7 =
    transition(CONST_CHAR('M'), CONST_FLOAT(0.0757404), emit_ohelixo7, state_outglob10) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.92426), emit_ohelixo7, state_out10)
    # h;
  emit_ohelixo7 =
    emission(CONST_FLOAT(0.110353), CHAR('A')) |
    emission(CONST_FLOAT(0.00498206), CHAR('C')) |
    emission(CONST_FLOAT(0.00469973), CHAR('D')) |
    emission(CONST_FLOAT(0.00649427), CHAR('E')) |
    emission(CONST_FLOAT(0.0973043), CHAR('F')) |
    emission(CONST_FLOAT(0.0737631), CHAR('G')) |
    emission(CONST_FLOAT(0.0119931), CHAR('H')) |
    emission(CONST_FLOAT(0.12167), CHAR('I')) |
    emission(CONST_FLOAT(0.00180854), CHAR('K')) |
    emission(CONST_FLOAT(0.157124), CHAR('L')) |
    emission(CONST_FLOAT(0.044721), CHAR('M')) |
    emission(CONST_FLOAT(0.0107496), CHAR('N')) |
    emission(CONST_FLOAT(0.0283821), CHAR('P')) |
    emission(CONST_FLOAT(0.0143416), CHAR('Q')) |
    emission(CONST_FLOAT(0.00857182), CHAR('R')) |
    emission(CONST_FLOAT(0.0402204), CHAR('S')) |
    emission(CONST_FLOAT(0.0501039), CHAR('T')) |
    emission(CONST_FLOAT(0.107462), CHAR('V')) |
    emission(CONST_FLOAT(0.0501891), CHAR('W')) |
    emission(CONST_FLOAT(0.0550665), CHAR('Y'))
    # h;

  state_ihelixo1 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixo7, state_ihelixo2)
    # h;

  state_ihelixo2 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixo7, state_ihelixo3)
    # h;

  state_ihelixo3 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixo7, state_ihelixo4)
    # h;

  state_ihelixo4 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixo7, state_ihelixo5)
    # h;

  state_ihelixo5 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixo7, state_ihelixo6)
    # h;

  state_ihelixo6 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelixo7)
    # h;

  state_ihelixo7 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelixm)
    # h;

  state_ihelixm =
    transition(CONST_CHAR('M'), CONST_FLOAT(0.000534023), emit_ohelixm, state_ihelix2) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.000567583), emit_ohelixm, state_ihelix3) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00064457), emit_ohelixm, state_ihelix4) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00159544), emit_ohelixm, state_ihelix5) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.000669433), emit_ohelixm, state_ihelix6) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00161008), emit_ohelixm, state_ihelix7) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.000762887), emit_ohelixm, state_ihelix8) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.000789084), emit_ohelixm, state_ihelix9) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.000868204), emit_ohelixm, state_ihelix10) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00519996), emit_ohelixm, state_ihelix11) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00774891), emit_ohelixm, state_ihelix12) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.0108947), emit_ohelixm, state_ihelix13) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.697722), emit_ohelixm, state_ihelix14) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.0444777), emit_ohelixm, state_ihelix15) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.0328707), emit_ohelixm, state_ihelix16) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.111827), emit_ohelixm, state_ihelix17) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.0324248), emit_ohelixm, state_ihelix18) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.0448694), emit_ohelixm, state_ihelix19) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00212234), emit_ohelixm, state_ihelix20) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00107586), emit_ohelixm, state_ihelix21) |
    transition(CONST_CHAR('M'), CONST_FLOAT(0.00072618), emit_ohelixm, state_ihelixi1)
    # h;

  state_ihelix2 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix3)
    # h;

  state_ihelix3 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix4)
    # h;

  state_ihelix4 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix5)
    # h;

  state_ihelix5 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix6)
    # h;

  state_ihelix6 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix7)
    # h;

  state_ihelix7 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix8)
    # h;

  state_ihelix8 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix9)
    # h;

  state_ihelix9 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix10)
    # h;

  state_ihelix10 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix11)
    # h;

  state_ihelix11 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix12)
    # h;

  state_ihelix12 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix13)
    # h;

  state_ihelix13 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix14)
    # h;

  state_ihelix14 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix15)
    # h;

  state_ihelix15 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix16)
    # h;

  state_ihelix16 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix17)
    # h;

  state_ihelix17 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix18)
    # h;

  state_ihelix18 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix19)
    # h;

  state_ihelix19 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix20)
    # h;

  state_ihelix20 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelix21)
    # h;

  state_ihelix21 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelixi1)
    # h;

  state_ihelixi1 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelixi2)
    # h;

  state_ihelixi2 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixm, state_ihelixi3)
    # h;

  state_ihelixi3 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixi1, state_ihelixi4)
    # h;

  state_ihelixi4 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixi1, state_ihelixi5)
    # h;

  state_ihelixi5 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixi1, state_ihelixi6)
    # h;

  state_ihelixi6 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixi1, state_ihelixi7)
    # h;

  state_ihelixi7 =
    transition(CONST_CHAR('M'), CONST_FLOAT(1.0), emit_ohelixi1, state_in10)
    # h;

  state_end = nil(EMPTY) # h;
}

instance dummy = gra_tmhmm(alg_enum);
instance fwd_scaled = gra_tmhmm(alg_fwd_scaled);
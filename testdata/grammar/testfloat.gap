signature sig_bst(alphabet, answer) {
  answer keypair(float);
  answer tcase(int);
  choice [answer] h([answer]);
}

algebra alg_enum auto enum;
algebra alg_count auto count;

algebra alg_test implements sig_bst(alphabet=char, answer=float) {
  float keypair(float x) {
    return x;
  }
  float tcase(int y) {
    return y;
  }
  choice [float] h([float] candidates) {
    return candidates;
  }

}

grammar gra_bst uses sig_bst(axiom = entry) {
  entry = keypair(FLOAT) | tcase(INT) # h;
}

//~ instance enum = gra_bst(alg_test);
instance enum = gra_bst(alg_enum * alg_test);

type Rope = extern

signature sig_test(alphabet, answer) {
  answer addx(alphabet, answer);
  answer addss(Rope, answer);
  answer addint(int, answer);
  answer addconst(int, alphabet, answer);
  answer nil(void);
  choice [answer] h([answer]);
}

algebra alg_enum auto enum;
algebra alg_count auto count;

grammar gra_test uses sig_test(axiom=A) {
  A = addconst(CONST_INT(-98765), CHAR('x'), A) | addx(CHAR('x'), A) | addss(ROPE("stefan"), A) | addint(INT, A) | nil(EMPTY) # h;
}

instance testme = gra_test(alg_enum);

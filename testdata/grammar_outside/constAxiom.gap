signature sig_test(alphabet, answer) {
  answer silent(char, answer);
  answer consume_left(char, alphabet, answer);
  answer consume_right(char, answer, alphabet);
  answer dummy(answer);
  answer nil(char, void);
  choice [answer] h([answer]);
}
    
algebra alg_enum auto enum;

algebra alg_count auto count;

grammar gra_test uses sig_test(axiom = Sm1) {
  // tabulated {S9}
  Sm1 = dummy(S0)
      # h;

  S0 = silent(CONST_CHAR('A'), S9)
     | silent(CONST_CHAR('B'), S1)
     # h;
     
  S9 = nil(CONST_CHAR('Z'), EMPTY)
     # h;
   
  S1 = consume_left(CONST_CHAR('C'), CHAR, S1)
     | consume_right(CONST_CHAR('D'), S1, CHAR)
     | silent(CONST_CHAR('E'), S9)
     # h;
}
    
instance count = gra_test(alg_count);
instance enum = gra_test(alg_enum);

    


signature Bill(alphabet, answer) {
  answer f1(Subsequence);
  answer f2(Subsequence, Subsequence);
  answer f3(answer, Subsequence);
  answer g1(alphabet);
  answer g2(alphabet, alphabet);

  choice [answer] h([answer]);
}

algebra buyer implements Bill(alphabet = char, answer = int) {

  int f1(Subsequence a) { return 3; }
  int f2(Subsequence a, Subsequence b) { return 3; }
  int f3(int a, Subsequence b) { return 3; }
  int g1(char a) { return 3; }
  int g2(char a, char b) { return 3; }

  choice [int] h([int] i)
  {
    return list(minimum(i));
  }
}

algebra enum auto enum;

// Alt::Link::optimize_choice etc. should not propagate choice
// optimization to the foo non-terminal
// fixed bug - was: non-compilable C++-code for this grammar

grammar bill uses Bill (axiom=formula) {

  formula = f1(REGION) | f2(REGION, REGION) | f3(foo, REGION) # h ;

  foo = g1(CHAR) | g2(CHAR, CHAR) ; //# h;

}


instance buyer = bill ( buyer ) ;
instance enu = bill(enum);

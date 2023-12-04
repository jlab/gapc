
signature Bill(alphabet, answer) {

  
  answer f(int);
  answer add(answer, alphabet, answer);
  answer mult(answer, alphabet, answer);
  choice [answer] h([answer]);
}


algebra buyer implements Bill(alphabet = char, answer = int) {

  int f(int i) { return i; }

  int add(int i, char c, int j)
  {
    return i + j;
  }

  int mult(int i, char c, int j)
  {
    return i * j;
  }

  choice [int] h([int] i)
  {
    return list(minimum(i));
  }
}

algebra count auto count ;
algebra enum auto enum ;

grammar bill uses Bill (axiom=formula) {

  tabulated { formula, number }

  formula = number |
            add(formula, plus, formula) |
            mult(formula, times, formula) # h ;

  number = f(INT);

  hirnverband = f(INT);

  plus = CHAR('+') ;
  times = CHAR('*') ;

}


instance count = bill ( count ) ;

instance buyer = bill ( buyer ) ;


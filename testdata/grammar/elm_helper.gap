
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

  int helper0(void)
  {
    return 42;
  }

  int helper00()
  {
    return 42;
  }

  choice [int] h([int] i)
  {
    // FIXME
    //int a = helper0();
    int b = helper00();
    return list(minimum(i));
  }

  int helper(int x)
  {
    return 42;
  }
}


grammar bill uses Bill (axiom=formula) {

  tabulated { formula, number }

  formula = number |
            add(formula, plus, formula) |
            mult(formula, times, formula) # h ;

  number = f(INT);

  plus = CHAR('+') ;
  times = CHAR('*') ;

}


instance buyer = bill ( buyer ) ;

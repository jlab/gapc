

signature Bill(alphabet, answer) {
  answer f(int);
  answer add(answer, alphabet, answer);
  answer mult(answer, alphabet, answer);
  choice [answer] h([answer]);
}



algebra count auto count ;

grammar bill uses Bill (axiom=formula) {

  formula = number |
            add(formula, plus, formula) |
            mult(formula, times, formula) with_overlay samesize # h ;

  number = f(INT);

  plus = CHAR('+') ;
  times = CHAR('*') ;

}

instance count = bill ( count ) ;



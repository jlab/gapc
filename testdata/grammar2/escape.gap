signature Bill(alphabet, answer) {
  answer f(alphabet);
  choice [answer] h([answer]);
}

algebra acount auto count ;

grammar bill uses Bill (axiom=formula) {

  formula = 
      f(CHAR('\0')) | f(CHAR('\1')) # h ;
      
}

instance acount = bill(acount) ;

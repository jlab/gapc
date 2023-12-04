
signature Bill(alphabet, answer) {

  
  answer f(int);
  answer add(answer, alphabet, answer);
  answer mult(answer, alphabet, answer);
  choice [answer] h([answer]);
  choice [answer] hx([answer]);
}


// types scoreing, synoptic, classification, printing, pretty printing

//scoring algebra buyer implements Bill(alphabet = char, answer = int) {
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
  scoring choice [int] hx([int] i)
  {
    return list(maximum(i));
  }
}


//pretty algebra pretty implements Bill(alphabet = char, answer = string) 
algebra pretty implements Bill(alphabet = char, answer = string) 
{
  string f(int i) {
    string r;
    append(r, i);
    return r;
  }

  string add(string i, char c, string j)
  {
    string r;
    append(r, '(');
    append(r, i);
    append(r, c);
    append(r, j);
    append(r, ')');
    return r;
  }

  string mult(string i, char c, string j)
  {
    string r;
    append(r, '(');
    append(r, i);
    append(r, c);
    append(r, j);
    append(r, ')');
    return r;
  }

  choice [string] h([string] i)
  {
    return i;
  }
  choice [string] hx([string] i)
  {
    return i;
  }
}


algebra acount auto count ;
algebra enum auto enum ;

grammar bill uses Bill (axiom=formula) {

  tabulated { formula, number }

  formula = number |
            foo |
            bar # h ;

  foo = add(formula, plus, formula) # h ;

  bar = mult(formula, times, formula) # hx ;

  number = f(INT);

  plus = CHAR('+') ;
  times = CHAR('*') ;

}



instance count = bill (acount ) ;

instance enum = bill ( enum ) ;

instance buyer = bill ( buyer ) ;

instance buyerpp = bill ( buyer * pretty ) ;



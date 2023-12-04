
type nosuchtype = extern

signature Bill(alphabet, answer) {

  
  answer f(int);
  answer add(answer, alphabet, answer);
  answer mult(answer, alphabet, answer);
  choice [answer] h([answer]);
}



algebra buyer implements Bill(alphabet = char, answer = nosuchtype) {

  nosuchtype f(int i) { return i; }

  nosuchtype add(nosuchtype i, char c, nosuchtype j)
  {
    return i + j;
  }

  nosuchtype mult(nosuchtype i, char c, nosuchtype j)
  {
    return i * j;
  }

  choice [nosuchtype] h([nosuchtype] i)
  {
    return list(minimum(i));
  }
}

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


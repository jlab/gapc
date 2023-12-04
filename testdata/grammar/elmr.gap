
import rational

signature Bill(alphabet, answer) {

  
  answer f(rational);
  answer add(answer, alphabet, answer);
  answer mult(answer, alphabet, answer);
  choice [answer] h([answer]);
}


algebra buyer implements Bill(alphabet = char, answer = rational) {

  rational f(rational i) { return i; }

  rational add(rational i, char c, rational j)
  {
    return i + j;
  }

  rational mult(rational i, char c, rational j)
  {
    return i * j;
  }

  choice [rational] h([rational] i)
  {
    return list(minimum(i));
  }
}

//pretty algebra pretty implements Bill(alphabet = char, answer = string) 
algebra pretty implements Bill(alphabet = char, answer = string) 
{
  string f(rational i) {
    string r;
    //append(r, i);
    append(r, '_');
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


algebra acount auto count ;
algebra enum auto enum ;

grammar bill uses Bill (axiom=formula) {

  tabulated { formula, number }

  formula = number |
            add(formula, plus, formula) |
            mult(formula, times, formula) # h ;

  number = f(CONST_RATIO(1 $ 4));

  plus = CHAR('+') ;
  times = CHAR('*') ;

}


instance buyer = bill ( buyer ) ;

instance buyerpp = bill ( buyer * pretty ) ;



type Rope = extern

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

algebra seller extends buyer {
  choice [int] h([int] l)
  {
    return list(maximum(l));
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
}



algebra count auto count ;
algebra enum auto enum ;

grammar bill uses Bill (axiom=formula) {

  tabulated { formula, number }

  formula = number |

            // check for codegen block filter bug

            { add(formula, plus, formula) } with maxsize(0) |
            mult(formula, times, formula) # h ;

  number = f(INT);

  plus = CHAR('+') ;
  times = CHAR('*') ;

}

instance count = bill ( count ) ;


instance seller = bill ( seller ) ;

instance buyer = bill ( buyer ) ;

instance buyerpp = bill ( buyer * pretty ) ;

instance sellercnt = bill ( seller * count ) ;


instance enum = bill ( enum ) ;


instance foo = bill ( buyer ) ;

instance fu = bill (  buyer * pretty ) ;

instance sc = bill (  seller * count ) ;

instance ex = bill ( pretty ) ;



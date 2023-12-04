
type Rope = extern

signature Bill(alphabet, answer) {
  answer f(int);
  answer add(answer, alphabet, answer);
  answer mult(answer, alphabet, answer);
  choice [answer] h([answer]);
}

algebra count implements Bill
  (alphabet = char,
   answer = int) {

  int f(int i) { return 1; }

  int add(int i, char c, int j)
  {
    return i * j;
  }

  int mult(int i, char c, int j)
  {
    return i * j;
  }

  choice [int] h([int] i)
  {
    return list(sum(i));
  }

}


algebra buyer implements Bill(alphabet = char, answer = int) {

  int f(int i) { return i; }

  int add(int i, char c, int j)
  {
    return i + j;
    for ( int i=0 ; i==i; i=i+1)  break; 
  }

  int mult(int i, char c, int j)
  {
    return i * j;
    while (1) break;
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

algebra time implements Bill(alphabet = char, answer = int) {

  int f(int i) { return 0; }

  int add(int i, char c, int j)
  {
    if (i > j)
      return i + 2;
    return j + 2;
  }

  int mult(int i, char c, int j)
  {
    if (i > j)
      return i + 5;
    return j + 5;
  }

  choice [int] h([int] i)
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

algebra pretty2 implements Bill(alphabet = char, answer = Rope) 
{
  Rope f(int i) {
    Rope r;
    append(r, i);
    return r;
  }

  Rope add(Rope i, char c, Rope j)
  {
    return '(' + i + c + j + ')';
  }

  Rope mult(Rope i, char c, Rope j)
  {
    return '(' + i + c + j + ')';
  }

  choice [Rope] h([Rope] i)
  {
    return i;
  }
}

algebra acount auto count ;
algebra enum auto enum ;

grammar bill uses Bill (axiom=formula) {

  // Uncomment for an explicit table configuration
  // instead of deriving a good one via 'gapc -t'
  // tabulated { formula, number }

  formula = number |
            add(formula, plus, formula) |
           // mult(formula, times, formula) |
             foo with minsize(1)  
//             foo 
# h ;
  foo = add(formula, plus, formula) |
            mult(formula, times, formula)  ;

  number = f(INT);

  plus = CHAR('+') ;
  times = CHAR('*') ;

}

instance count = bill ( count ) ;

instance acount = bill (acount ) ;

instance pretty2 = bill ( pretty2 );

instance seller = bill ( seller ) ;

instance buyer = bill ( buyer ) ;

instance buyerpp = bill ( buyer * pretty ) ;

instance sellercnt = bill ( seller * count ) ;

instance time = bill ( time ) ;

instance timecnt = bill ( time * count ) ;

instance timesellerpp = bill ( time * seller * pretty ) ;

instance timebuyerpp = bill ( time * buyer * pretty ) ;

instance enum = bill ( enum ) ;

instance foo = bill ( buyer ) ;

instance fu = bill (  buyer * pretty ) ;

instance sc = bill (  seller * count ) ;

instance ex = bill ( pretty ) ;

instance enumenum = bill ( enum * enum ) ;


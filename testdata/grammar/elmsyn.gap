
type Rope = extern

signature Bill(alphabet, answer) {

  
  answer f(int);
  answer add(answer, alphabet, answer);
  answer mult(answer, alphabet, answer);
  choice [answer] h([answer]);
}


// types scoreing, synoptic, classification, printing, pretty printing

//synoptic algebra count implements Bill
algebra count(int k = 2) implements Bill
  (alphabet = char /* blah blah */,
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

//classify algebra klass implements Bill(alphabet = char, answer = string) 
algebra klass implements Bill(alphabet = char, answer = string) 
{
  string f(int i) {
    return int2string(i);
  }

  string add(string i, char c, string j)
  {
    return string_concat(string_concat(i, char_to_string(c)), j);
  }

  string mult(string i, char c, string j)
  {
    return string_concat(string_concat(i, char_to_string(c)), j);
  }

  choice [string] h([string] i)
  {
    return unique(i);
  }
}

algebra acount auto count ;
algebra enum auto enum ;

grammar bill uses Bill (axiom=formula) {

  tabulated { formula, number }

  formula = number |
            add(formula, plus, formula) |
            mult(formula, times, formula) # h ;

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

instance t(k) = bill (  (klass * count) ) ;

instance affe(k) = bill (  (seller * pretty * pretty) ) ;

// instance bar(k) = bill (  (buyer / pretty) (k = 3) ) ;

instance inst(k) = bill ( seller {k = 3} ) ;

instance enumenum = bill ( enum * enum ) ;


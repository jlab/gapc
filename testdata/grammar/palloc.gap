signature palin(alphabet, answer) {
  answer match(alphabet, answer, alphabet);
  answer nil(int);
  answer skipl(char, answer);
  answer skipr(answer, char);
  choice [answer] h([answer]);
}

algebra pretty implements palin(alphabet = char, answer = string) {

  string match(char a, string b, char c)
  {
    string r;
    append(r, a);
    append(r, b);
    return r;
  }

  string nil(int l)
  {
    string r;
    return r;
  }

  string skipl(char c, string x)
  {
    return x;
  }

  string skipr(string x, char c)
  {
    return x;
  }

  choice [string] h([string] x)
  {
    return x;
  }
  
}

algebra count auto count ;

algebra score implements palin(alphabet = char, answer = int) {

  int match(char a, int b, char c)
  {
    return b + 1;
  }

  int nil(int l)
  {
    return 0;
  }

  int skipl(char c, int x)
  {
    return x;
  }

  int skipr(int x, char c)
  {
    return x;
  }

  choice [int] h([int] x)
  {
    return list(maximum(x));
  }
  
}

grammar pal uses palin(axiom = sl)
{

  tabulated { pl }

  sl = sr |
       skipl(CHAR, sl) # h ;

  sr = skipr(sr, CHAR) |
       pl # h ;

  pl = match(CHAR, pl, CHAR) with equal |
       nil(SEQ1) # h ;

}

instance pretty = pal ( pretty ) ; 

instance count = pal ( count ) ; 

instance score = pal ( score ) ;

instance scorepp = pal ( score * pretty ) ;

instance scorecnt = pal ( score * count ) ;


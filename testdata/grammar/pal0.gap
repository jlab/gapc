
// Even length palindrome

signature palin(alphabet, answer) {
  answer match(alphabet, answer, alphabet);
  answer nil(void);
  choice [answer] h([answer]);
}

algebra enum auto enum ;

algebra pretty implements palin(alphabet = char, answer = string) {

  string match(char a, string b, char c)
  {
    string r;
    append(r, a);
    append(r, b);
    append(r, c);
    return r;
  }

  string nil(void)
  {
    string r;
    append(r, '|');
    return r;
  }

  choice [string] h([string] x)
  {
    return x;
  }
  
}


algebra score implements palin(alphabet = char, answer = int) {

  int match(char a, int b, char c)
  {
    return b + 1;
  }

  int nil(void)
  {
    return 0;
  }

  choice [int] h([int] x)
  {
    return list(maximum(x));
  }
  
}

grammar pal uses palin(axiom = pl)
{

  pl = match(CHAR, pl, CHAR) with equal |
       nil(EMPTY) # h ;

}

instance pretty = pal ( pretty ) ; 

instance score = pal ( score ) ;

instance scorepp = pal ( score * pretty ) ;

instance enum = pal ( enum );


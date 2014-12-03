
signature Nuss(alphabet, answer) {

  answer nil(void);
  answer right(answer, alphabet);
  answer left(alphabet, answer);
  answer pair(alphabet, answer, alphabet);
  answer split(answer, answer);
  choice [answer] h([answer]);

}

algebra pretty implements Nuss(alphabet = char, answer = string)
{
  string nil(void)
  {
    string r;
    return r;
  }

  string right(string a, char c)
  {
    string r;
    append(r, a);
    append(r, '.');
    return r;
  }

  string left(char c, string a)
  {
    string r;
    append(r, '.');
    append(r, a);
    return r;
  }

  string pair(char c, string m, char d)
  {
    string r;
    append(r, '(');
    append(r, m);
    append(r, ')');
    return r;
  }

  string split(string l, string r)
  {
    string res;
    append(res, l);
    append(res, r);
    return res;
  }

  choice [string] h([string] l)
  {
    return l;
  }
  
}

algebra bpmax implements Nuss(alphabet = char, answer = int)
{
  int nil(void)
  {
    return 0;
  }

  int right(int a, char c)
  {
    return a;
  }

  int left(char c, int a)
  {
    return a;
  }

  int pair(char c, int m, char d)
  {
    return m + 1;
  }

  int split(int l, int r)
  {
    return l + r;
  }

  choice [int] h([int] l)
  {
    return list(maximum(l));
  }
  
}

algebra count auto count ;


grammar durbin uses Nuss (axiom=start) {

  tabulated { start }

  start = nil(EMPTY)                                                  |
          right(start, CHAR)                                          |
          left(CHAR, start)                                          |
          split(start with minsize (1), start with minsize (1))       |
          pair(CHAR, start, CHAR) with char_basepairing # h ;

}

instance pretty = durbin ( pretty ) ;

instance bpmax = durbin ( bpmax ) ;

instance bpmaxpp = durbin ( bpmax * pretty ) ;

instance count = durbin ( count ) ;

instance bpmaxcnt = durbin ( bpmax * count ) ;





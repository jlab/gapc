

type spair = ( string first, string second )

signature Align(alphabet, answer) {
  answer match(alphabet, answer, alphabet);
  answer del(alphabet, answer);
  answer ins(answer, alphabet);
  answer delx(alphabet, answer);
  answer insx(answer, alphabet);

  answer nil(int);
  answer sl(char, answer);
  answer sr(answer, char);
  choice [answer] h([answer]);
}

algebra count auto count ;

algebra pretty implements Align(alphabet = char, answer = spair ) {
  spair match(char a, spair m, char b)
  {
    spair r;
    append(r.first, a);
    append(r.first, m.first);
    append(r.second, b);
    append(r.second, m.second);
    return r;
  }

  spair del(char a, spair m)
  {
    spair r;
    append(r.first, a);
    append(r.first, m.first);
    append(r.second, '=');
    append(r.second, m.second);
    return r;
  }

  spair ins(spair m, char b)
  {
    spair r;
    append(r.first, '=');
    append(r.first, m.first);
    append(r.second, b);
    append(r.second, m.second);
    return r;
  }

  spair delx(char a, spair m)
  {
    spair r;
    append(r.first, a);
    append(r.first, m.first);
    append(r.second, '-');
    append(r.second, m.second);
    return r;
  }

  spair insx(spair m, char b)
  {
    spair r;
    append(r.first, '-');
    append(r.first, m.first);
    append(r.second, b);
    append(r.second, m.second);
    return r;
  }


  spair nil(int l)
  {
    spair r;
    return r;
  }

  spair sl(char a, spair m)
  {
    return m;
  }

  spair sr(spair m, char b)
  {
    return m;
  }

  choice [spair] h([spair] l)
  {
    return l;
  }

}

algebra affine implements Align(alphabet = char, answer = int ) {
  int match(char a, int m, char b)
  {
    if (a == b)
      return m + 4;
    else
      return m - 3;
  }

  int del(char a, int m)
  {
    return m - 15 - 1;
  }

  int ins(int m, char b)
  {
    return m - 15 - 1;
  }

  int delx(char a, int m)
  {
    return m - 1;
  }

  int insx(int m, char b)
  {
    return m - 1;
  }


  int nil(int l)
  {
    return 0;
  }

  int sl(char a, int m)
  {
    return m;
  }

  int sr(int m, char b)
  {
    return m;
  }

  choice [int] h([int] l)
  {
    return list(maximum(l));
  }

}
grammar affinelocsim uses Align(axiom = skipR)
{

  tabulated { alignment, xDel, xIns }

  skipR = sr(skipR, CHAR_SEP) |
          skipL # h ;

  skipL = sl(CHAR_SEP, skipL) |
          alignment # h ;

  alignment = nil(SEQ)   |
              del(CHAR_SEP, xDel) |
              ins(xIns, CHAR_SEP) |
              match(CHAR_SEP, alignment, CHAR_SEP) # h ;

  xDel = alignment |
         delx(CHAR_SEP, xDel) # h ;

  xIns = alignment |
         insx(xIns, CHAR_SEP) # h ;

}

instance pretty = affinelocsim(pretty);

instance count = affinelocsim ( count ) ; 

instance affine = affinelocsim ( affine ) ;

instance affinecnt = affinelocsim ( affine * count ) ;

instance affinepp = affinelocsim ( affine * pretty ) ;


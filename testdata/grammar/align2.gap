
input < raw, raw >

type spair = ( string first, string second )

signature Align(alphabet, answer) {
  answer match( < alphabet, alphabet >, answer);
  answer del( < alphabet, void >, answer);
  answer ins( < void, alphabet >, answer);
  answer nil( <int, int> );
  choice [answer] h([answer]);
}

algebra count auto count ;

algebra pretty implements Align(alphabet = char, answer = spair ) {
  spair match(< char a, char b>, spair m)
  {
    spair r;
    append(r.first, a);
    append(r.first, m.first);
    append(r.second, b);
    append(r.second, m.second);
    return r;
  }

  spair del(<char a, void>,  spair m)
  {
    spair r;
    append(r.first, a);
    append(r.first, m.first);
    append(r.second, '=');
    append(r.second, m.second);
    return r;
  }

  spair ins(<void , char b>, spair m)
  {
    spair r;
    append(r.first, '=');
    append(r.first, m.first);
    append(r.second, b);
    append(r.second, m.second);
    return r;
  }

  spair nil(<int l, int m>)
  {
    spair r;
    return r;
  }


  choice [spair] h([spair] l)
  {
    return l;
  }

}

algebra affine implements Align(alphabet = char, answer = int ) {
  int match(<char a, char b>, int m)
  {
    if (a == b)
      return m + 4;
    else
      return m - 3;
  }

  int del(<char a, void>, int m)
  {
    return m - 15 - 1;
  }

  int ins(<void, char b>, int m)
  {
    return m - 15 - 1;
  }

  int nil(<int l, int m>)
  {
    return 0;
  }

  choice [int] h([int] l)
  {
    return list(maximum(l));
  }

}
grammar affinelocsim uses Align(axiom = alignment)
{


  alignment = nil( < SEQ, SEQ> )   |
              del( < CHAR, EMPTY >, alignment) |
              ins( < EMPTY, CHAR > , alignment) |
              match( < CHAR, CHAR >, alignment) # h ;


}

instance affine = affinelocsim ( affine ) ;

instance pretty = affinelocsim(pretty);

instance count = affinelocsim ( count ) ; 

instance affinecnt = affinelocsim ( affine * count ) ;

instance affinepp = affinelocsim ( affine * pretty ) ;


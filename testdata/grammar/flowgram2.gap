
import helper

input < raw, raw >

type Rope = extern
type spair = ( Rope first, Rope second )
type ipair = ( int first, int second )

signature Align(alphabet, answer) {
  answer r( <alphabet, alphabet>, answer );
  answer del( <alphabet, void>, <alphabet, void>,
              <alphabet, void>, <alphabet, void>, answer);
  answer ins( <void, alphabet>, <void, alphabet>,
              <void, alphabet>, <void, alphabet>, answer);
  answer ti( <void, int>);
  answer td( <int, void>);
  answer nil( <void, void>);
  choice [answer] h([answer]);
}

//algebra count auto count ;

algebra pretty implements Align(alphabet = single, answer = spair ) {
  spair r( <single a, single b>, spair m)
  {
    spair r;
    append(r.first, a);
    append(r.first, ' ');
    append(r.first, m.first);

    append(r.second, b);
    append(r.second, ' ');
    append(r.second, m.second);
    return r;
  }

  spair del( <single a, void>, <single b, void>,
             <single c, void>, <single d, void>, spair m)
  {
    spair r;
    append(r.first, a);
    append(r.first, ' ');
    append(r.first, b);
    append(r.first, ' ');
    append(r.first, c);
    append(r.first, ' ');
    append(r.first, d);
    append(r.first, ' ');
    append(r.first, m.first);
    append(r.second, "- - - - ");
    append(r.second, m.second);
    return r;
  }

  spair ins( <void, single a>, <void, single b>,
             <void, single c>, <void, single d>, spair m)
  {
    spair r;
    append(r.first, "- - - - ");
    append(r.first, m.first);
    append(r.second, a);
    append(r.second, ' ');
    append(r.second, b);
    append(r.second, ' ');
    append(r.second, c);
    append(r.second, ' ');
    append(r.second, d);
    append(r.second, ' ');
    append(r.second, m.second);
    return r;
  }

  spair ti( <void, int s> )
  {
    spair r;
    append(r.second, '=', s);
    return r;
  }

  spair td( <int s, void> )
  {
    spair r;
    append(r.first, '=', s);
    return r;
  }

  spair nil( <void, void> )
  {
    spair r;
    return r;
  }

  choice [spair] h([spair] l)
  {
    return l;
  }

}

algebra score implements Align(alphabet = single, answer = single ) {
  single r( <single a, single b>, single m)
  {
    return m + fabs(a-b);
  }

  single del( <single a, void>, <single b, void>,
              <single c, void>, <single d, void>, single m)
  {
    return m + 15.0 * 4.0;
  }

  single ins( <void, single a>, <void, single b>,
              <void, single c>, <void, single d>, single m)
  {
    return m + 15.0 * 4.0;
  }

  single ti( <void, int b>)
  {
    return 0.0;
  }

  single td( <int a, void>)
  {
    return 0.0;
  }

  single nil( <void, void> )
  {
    return 0.0;
  }

  choice [single] h([single] l)
  {
    return list(minimum(l));
  }

}

algebra length_alg implements Align(alphabet = single, answer = int ) {
  int r(<single a, single b>, int m)
  {
    return m + 1;
  }

  int del(<single a, void>, <single b, void>,
          <single c, void>, <single d, void>, int m)
  {
    return m + 4;
  }

  int ins(<void, single a>, <void, single b>,
          <void, single c>, <void, single d>, int m)
  {
    return m + 4;
  }

  int ti(<void, int b>)
  {
    return 0;
  }

  int td(<int a, void>)
  {
    return 0;
  }

  int nil(<void, void>)
  {
    return 0;
  }

  choice [int] h([int] l)
  {
    return l;
  }

}

algebra mismatch_seqlen implements Align(alphabet = single, answer = ipair ) {
  ipair r(<single a, single b>, ipair m)
  {
    ipair r;
    r.first = m.first + abs(myround(a) - myround(b));
    r.second = m.second + max(myround(a), myround(b));
    return r;
  }

  ipair del(<single a, void>, <single b, void>,
            <single c, void>, <single d, void>, ipair m)
  {
    ipair r;
    int x = myround(a) + myround(b) + myround(c) + myround(d);
    r.first = m.first + x;
    r.second = m.second + x;
    return r;
  }

  ipair ins(<void, single a>, <void, single b>,
            <void, single c>, <void, single d>, ipair m)
  {
    ipair r;
    int x = myround(a) + myround(b) + myround(c) + myround(d);
    r.first = m.first + x;
    r.second = m.second + x;
    return r;
  }

  ipair ti(<void, int b>)
  {
    ipair r;
    r.first = 0;
    r.second = 0;
    return r;
  }

  ipair td(<int a, void>)
  {
    ipair r;
    r.first = 0;
    r.second = 0;
    return r;
  }

  ipair nil(<void, void>)
  {
    ipair r;
    r.first = 0;
    r.second = 0;
    return r;
  }

  choice [ipair] h([ipair] l)
  {
    return l;
  }

}

grammar flow uses Align(axiom = ali)
{

  ali = nil( <EMPTY, EMPTY> )                                         |
        r( <CHAR, CHAR>, ali)                           |
        del( <CHAR, EMPTY>, <CHAR, EMPTY>, <CHAR, EMPTY>, <CHAR, EMPTY>, ali) |
        ins( <EMPTY, CHAR>, <EMPTY, CHAR>, <EMPTY, CHAR>, <EMPTY, CHAR>, ali ) |
        ti( < EMPTY, SEQ1 > )                               |
        td( < SEQ1, EMPTY > )
                                                         # h ;

}

instance score = flow(score);
instance pretty = flow(pretty);
instance sp = flow(score*pretty);
instance foo = flow(score*(length_alg*mismatch_seqlen));



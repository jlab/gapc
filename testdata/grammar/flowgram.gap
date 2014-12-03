
import helper

type Rope = extern
type spair = ( Rope first, Rope second )
type ipair = ( int first, int second )

signature Align(alphabet, answer) {
  answer r(alphabet, answer, alphabet);
  answer del(alphabet, alphabet, alphabet, alphabet, answer);
  answer ins(answer, alphabet, alphabet, alphabet, alphabet);
  answer ti(alphabet, int);
  answer td(int, alphabet);
  answer nil(alphabet);
  choice [answer] h([answer]);
}

//algebra count auto count ;

algebra pretty implements Align(alphabet = single, answer = spair ) {
  spair r(single a, spair m, single b)
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

  spair del(single a, single b, single c, single d, spair m)
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

  spair ins(spair m, single a, single b, single c, single d)
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

  spair ti(single a, int s)
  {
    spair r;
    append(r.second, '=', s);
    return r;
  }

  spair td(int s, single a)
  {
    spair r;
    append(r.first, '=', s);
    return r;
  }

  spair nil(single l)
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
  single r(single a, single m, single b)
  {
    return m + fabs(a-b);
  }

  single del(single a, single b, single c, single d, single m)
  {
    return m + 15.0 * 4.0;
  }

  single ins(single m, single a, single b, single c, single d)
  {
    return m + 15.0 * 4.0;
  }

  single ti(single a, int s)
  {
    return 0.0;
  }

  single td(int s, single a)
  {
    return 0.0;
  }

  single nil(single l)
  {
    return 0.0;
  }

  choice [single] h([single] l)
  {
    return list(minimum(l));
  }

}

algebra length_alg implements Align(alphabet = single, answer = int ) {
  int r(single a, int m, single b)
  {
    return m + 1;
  }

  int del(single a, single b, single c, single d, int m)
  {
    return m + 4;
  }

  int ins(int m, single a, single b, single c, single d)
  {
    return m + 4;
  }

  int ti(single a, int s)
  {
    return 0;
  }

  int td(int s, single a)
  {
    return 0;
  }

  int nil(single l)
  {
    return 0;
  }

  choice [int] h([int] l)
  {
    return l;
  }

}

algebra mismatch_seqlen implements Align(alphabet = single, answer = ipair ) {
  ipair r(single a, ipair m, single b)
  {
    ipair r;
    r.first = m.first + abs(myround(a) - myround(b));
    r.second = m.second + max(myround(a), myround(b));
    return r;
  }

  ipair del(single a, single b, single c, single d, ipair m)
  {
    ipair r;
    int x = myround(a) + myround(b) + myround(c) + myround(d);
    r.first = m.first + x;
    r.second = m.second + x;
    return r;
  }

  ipair ins(ipair m, single a, single b, single c, single d)
  {
    ipair r;
    int x = myround(a) + myround(b) + myround(c) + myround(d);
    r.first = m.first + x;
    r.second = m.second + x;
    return r;
  }

  ipair ti(single a, int s)
  {
    ipair r;
    r.first = 0;
    r.second = 0;
    return r;
  }

  ipair td(int s, single a)
  {
    ipair r;
    r.first = 0;
    r.second = 0;
    return r;
  }

  ipair nil(single l)
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

  ali = nil(NON)                                         |
        r(CHAR_SEP, ali, CHAR_SEP)                           |
        del(CHAR_SEP, CHAR_SEP, CHAR_SEP, CHAR_SEP, ali) |
        ins(ali, CHAR_SEP, CHAR_SEP, CHAR_SEP, CHAR_SEP) |
        ti(NON, SEQ)                               |
        td(SEQ, NON)
                                                         # h ;

}

instance pretty = flow(pretty);
instance score = flow(score);
instance sp = flow(score*pretty);
instance foo = flow(score*(length_alg*mismatch_seqlen));



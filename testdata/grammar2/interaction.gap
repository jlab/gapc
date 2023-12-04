/*

EXAMPLE auauggg
EXAMPLE auauccc


HEADER
<h1>RNA-RNA-Interaction variant of Nussinov Algorithm</h1>

HEADER

*/

import interact

input < raw, raw >

/* type for pretty print result */
type spair = ( string first, string second )

signature Interact(alphabet, answer) {

  answer nil(void);
  answer unpaired(answer, alphabet);
  answer pair(alphabet, answer, alphabet);
  answer split(answer, answer);

  answer nil2(<void,void>);
  answer interact(answer,<alphabet,alphabet>);
  answer struct1(answer,<answer,void>);
  answer struct2(answer,<void,answer>);

  choice [answer] h([answer]);

}

algebra bpmax implements Interact(alphabet = char, answer = int)
{
  int nil(void)
  {
    return 0;
  }

  int unpaired(int a, char c)
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

  int nil2(<void, void>)
  {
      return 0;
  }

  int interact(int a, <char c, char d>)
  {
      return a + 1;
  }

  int struct1(int a, <int b, void>)
  {
      return a + b;
  }

  int struct2(int a, <void, int b>)
  {
      return a + b;
  }

  choice [int] h([int] l)
  {
    return list(maximum(l));
  }
  
}

algebra pretty implements Interact(alphabet = char, answer = spair ) {
  spair nil(void)
  {
      spair r;
      return r;
  }

  /* single sequences always use first component of pair */
  spair unpaired(spair a, char c)
  {
      spair r;
      append(r.first,a.first); 
      append(r.first,'.'); 
      return r;
      
  }

  spair pair(char c, spair m, char d)
  {
    spair r;
    append(r.first,'(');
    append(r.first,m.first);
    append(r.first,')');
    return r;
  }

  spair split(spair a, spair b)
  {
    spair r;
    append(r.first,a.first);
    append(r.first,b.first);
    return r;
  }

  spair nil2(<void, void>)
  {
    spair r;
    return r;
  }

  spair interact(spair a, <char c, char d>)
  {
    spair r;
    append(r.first,a.first);
    append(r.second,a.second);
    append(r.first,'|');  
    append(r.second,'|');  
    return r;
  }

  spair struct1(spair a, <spair b, void>)
  {
    spair r;
    append(r.first,a.first);
    append(r.second,a.second);
    append(r.first,b.first);
    return r;
  }

  spair struct2(spair a, <void, spair b>)
  {
    spair r;
    append(r.first,a.first);
    append(r.second,a.second);
    append(r.second,b.first);
    return r;
  }

  choice [spair] h([spair] l)
  {
    return l;
  }
}


grammar interaction uses Interact (axiom=I) {

  I = nil2(<EMPTY,EMPTY>) |
      interact(I,<CHAR,CHAR> with basepair ) |
      //interact(I,<CHAR,CHAR>) |
      struct1(I,<Nnonempty,EMPTY>)|
      struct2(I,<EMPTY,Nnonempty>) # h;
	
  N = nil(EMPTY) | Nnonempty # h;
  Nnonempty = unpaired(N, CHAR) |
          split(N, bp) # h ;

  bp = pair(CHAR, N, CHAR) with char_basepairing ;


}

instance bpmax = interaction ( bpmax );

instance pretty = interaction ( pretty );
instance bpmaxpp = interaction ( bpmax * pretty );



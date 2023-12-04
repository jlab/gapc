import rna
//import seqcon

input rna

type Rope = extern
type string_t = Rope
type triple7 = ( int mfe, int jr, int tl )

signature Algebra(alphabet, comp) {
  comp sr(Subsequence, comp, Subsequence);
  comp bl(Subsequence, Subsequence, comp, Subsequence);
  comp nil(void);
  choice [comp] h([comp]);
}

algebra mfetriple7 implements Algebra(alphabet = char, comp = triple7) {

  triple7 sr(Subsequence lb, triple7 x, Subsequence rb) {
    return x;
  }
  
  triple7 bl(Subsequence lb, Subsequence lr, triple7 x, Subsequence rb) {
    return x;
  }
  
  triple7 nil(void) {
    triple7 x;
    x.jr = 0;
    x.tl = 0;
    x.mfe = 0;
    return x;
  }
  
  choice [triple7] h([triple7] i) {
    return list(minimum(i));
  }
}

grammar p7 uses Algebra(axiom = startbulge) {
	
  startbulge	= bl (BASE, BASE, seqcon, BASE ) # h;
	
  seqcon	= sr (BASE, stack, BASE ) # h;
	
  stack		= sr (BASE, stack, BASE) | nil(EMPTY) # h;
}

instance mfe = p7(mfetriple7);

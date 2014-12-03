import rna

input rna

type Rope = extern

signature Algebra(alphabet, comp) {
  comp sadd(Subsequence, comp);
  comp nil(void);
  comp hl(Subsequence, Subsequence, Subsequence, Subsequence, Subsequence);
  choice [comp] h([comp]);
}

algebra mfe implements Algebra(alphabet = char, comp = int) {
  int sadd(Subsequence b, int x) {
    return x;
  }
  int nil(void) {
    return 0;
  }
  int hl(Subsequence llb, Subsequence lb, Subsequence r, Subsequence rb, Subsequence rrb) {
    return -10;
  }
  choice [int] h([int] i) {
    return list(minimum(i));
  }

}

algebra pretty implements Algebra(alphabet = char, comp = string) {
  string sadd(Subsequence b, string x) {
    string res;
    append(res, '.');
    append(res, x);
    return res;
  }
  string nil(void) {
    string res;
    return res;
  }
  string hl(Subsequence llb, Subsequence lb, Subsequence r, Subsequence rb, Subsequence rrb) {
    string res;
    append(res, "((", 2);
    append(res, '.', size(r));
    append(res, "))", 2);
    return res;
  }
  choice [string] h([string] i) {
    return i;
  }

}

grammar fold uses Algebra (axiom = structstart) {
 
 	 structstart = sadd( BASE , structstart ) | sadd( BASE , rnastruct )  # h;

	 rnastruct = hl( BASE , BASE , REGION with minsize(3) , BASE , BASE ) with basepairing # h ;


}
instance pretty = fold ( pretty );
instance mfe = fold ( mfe );
instance mfepre = fold ( mfe * pretty );

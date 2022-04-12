import rna

input rna

signature sig_foldrna(alphabet,answer) {
	answer nil(Subsequence); //empty structure
	answer bl(Subsequence, Subsequence, Subsequence, Subsequence); // a bulge loop to the left with a closing base-pair
	choice [answer] h([answer]);
}

algebra alg_count auto count;
algebra alg_enum auto enum;

algebra alg_mfe implements sig_foldrna(alphabet = char, answer = int) {
  int bl(Subsequence lb, Subsequence lr, Subsequence x, Subsequence rb) {
    return 2;
  }
  int nil(Subsequence n) {
    return 1;
  }
  choice [int] h([int] i) {
    return list(minimum(i));
  }
}

grammar gra_nodangle uses sig_foldrna(axiom = struct) {
  struct    = leftB |
              leftB with minsize(5)   |
              nil(LOC) # h;

  leftB     = bl(BASE, REGION, REGION, BASE) with basepairing # h;
}

instance ins = gra_nodangle(alg_mfe * alg_enum);
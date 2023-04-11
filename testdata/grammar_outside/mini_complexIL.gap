import rna

input rna

signature sig_foldrna(alphabet,answer) {
	answer nil(Subsequence); //empty structure
	choice [answer] h([answer]);
	answer il(Subsequence, Subsequence, answer, Subsequence, Subsequence); // an internal loop with a closing base-pair
    answer comp(Subsequence, Subsequence, Subsequence, Subsequence, answer, Subsequence, answer);
}

algebra alg_count auto count;
algebra alg_enum auto enum;

algebra alg_mfe implements sig_foldrna(alphabet = char, answer = int) {
  int nil(Subsequence n) {
    return 0;
  }
  choice [int] h([int] i) {
    return list(minimum(i));
  }
  int il(Subsequence lb, Subsequence lr, int x, Subsequence rr, Subsequence rb) {
    return x + il_energy(lr, rr);
  }
  int comp(Subsequence a, Subsequence lr,  Subsequence lr2, Subsequence lr3,  int x, Subsequence b, int y) {
    return x+y;
  }
}

//algebra alg_dotBracket implements sig_foldrna(alphabet = char, answer = string) {
//  string nil(Subsequence loc) {
//    string r;
//    return r;
//  }
//
//  choice [string] h([string] i) {
//    return i;
//  }
//
//  string il(Subsequence lb,Subsequence lregion,string e,Subsequence rregion,Subsequence rb) {
//    string res;
//    append(res, '(');
//    append(res, '.', size(lregion));
//    append(res, e);
//    append(res, '.', size(rregion));
//    append(res, ')');
//    return res;
//  }
//}

grammar gra_nodangle uses sig_foldrna(axiom = struct) {
    struct    = nil(LOC) |
                iloop
                # h;  
                
    iloop     = il(BASE, REGION, comp(REGION, BASE, BASE, REGION, struct, BASE, struct), REGION, BASE) with basepairing # h;

}

instance mfe = gra_nodangle(alg_mfe);

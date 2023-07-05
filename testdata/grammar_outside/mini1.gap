import rna

input rna

signature sig_foldrna(alphabet,answer) {
	answer sadd(Subsequence,answer); //add one unpaired base
    answer ssadd(Subsequence, answer);
	answer nil(Subsequence); //empty structure
	choice [answer] h([answer]);
}

algebra alg_count auto count;
algebra alg_enum auto enum;

algebra alg_mfe implements sig_foldrna(alphabet = char, answer = int) {
  int sadd(Subsequence lb, int x) {
    return x + sbase_energy();
  }
  int ssadd(Subsequence r, int x) {
        return x;
  }
  int nil(Subsequence n) {
    return 0;
  }
  choice [int] h([int] i) {
    return list(minimum(i));
  }
}

algebra alg_dotBracket implements sig_foldrna(alphabet = char, answer = string) {
  string sadd(Subsequence lb,string e) {
    string res;
    append(res, '.');
    append(res, e);
    return res;
  }
  
  string ssadd(Subsequence r, string e) {
    return e;
  }

  string nil(Subsequence loc) {
    string r;
    return r;
  }

  choice [string] h([string] i) {
    return i;
  }
}

grammar gra_nodangle uses sig_foldrna(axiom = struct) {
    struct    = nil(LOC) |
                sadd(BASE, struct)     |
                ssadd(REGION, struct)
                # h;  
                

}

instance mfe = gra_nodangle(alg_mfe);

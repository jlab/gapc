import rna

input rna

signature sig_foldrna(alphabet,answer) {
	answer sadd(Subsequence,answer); //add one unpaired base
	answer cadd(answer,answer); //adds one component, which has dangling bases from both sides, next component has a dangling base from left
	answer nil(Subsequence); //empty structure
	answer drem(Subsequence,answer,Subsequence); //no dangle, just the component
	answer sr(Subsequence,answer,Subsequence); //elongate a stack by one base-pair
	answer hl(Subsequence,Subsequence,Subsequence); //a hairpin loop with a closing base-pair
	answer bl(Subsequence, Subsequence, answer, Subsequence); // a bulge loop to the left with a closing base-pair
	answer br(Subsequence, answer, Subsequence, Subsequence); // a bulge loop to the right with a closing base-pair
	answer il(Subsequence, Subsequence, answer, Subsequence, Subsequence); // an internal loop with a closing base-pair
	answer ml(Subsequence,answer,Subsequence);  // a multi-loop with a closing base-pair and no dangling bases
	answer addss(answer,Subsequence); // append a region of unpaired bases
	answer incl(answer); // add penalty for one more multi-loop component
	choice [answer] h([answer]);
}

algebra alg_count auto count;
algebra alg_enum auto enum;

algebra alg_mfe implements sig_foldrna(alphabet = char, answer = int) {
  int sadd(Subsequence lb, int x) {
    return x + sbase_energy();
  }
  int cadd(int x, int y) {
    return x + y;
  }
  int drem(Subsequence lb, int x, Subsequence rb) {
    return x + termau_energy(lb, rb);
  }
  int sr(Subsequence lb, int x, Subsequence rb) {
    return x + sr_energy(lb, rb);
  }
  int hl(Subsequence lb, Subsequence r, Subsequence rb) {
    return     hl_energy(r);
  }
  int bl(Subsequence lb, Subsequence lr, int x, Subsequence rb) {
    return x + bl_energy(lr, rb);
  }
  int br(Subsequence lb, int x, Subsequence rr, Subsequence rb) {
    return x + br_energy(lb, rr);
  }
  int il(Subsequence lb, Subsequence lr, int x, Subsequence rr, Subsequence rb) {
    return x + il_energy(lr, rr);
  }
  int ml(Subsequence lb, int x, Subsequence rb) {
    return x + ml_energy() + ul_energy() + termau_energy(lb, rb);
  }
  int incl(int x) {
    return x + ul_energy();
  }
  int addss(int x, Subsequence r) {
    return x + ss_energy(r);
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

  string cadd(string le,string re) {
    string res;
    append(res, le);
    append(res, re);
    return res;
  }

  string nil(Subsequence loc) {
    string r;
    return r;
  }

  string drem(Subsequence lloc, string e, Subsequence rloc) {
    return e;
  }

  string sr(Subsequence lb,string e,Subsequence rb) {
    string res;
    append(res, '(');
    append(res, e);
    append(res, ')');
    return res;
  }

  string hl(Subsequence lb,Subsequence region,Subsequence rb) {
    string res;
    append(res, '(');
    append(res, '.', size(region));
    append(res, ')');
    return res;
  }


  string bl(Subsequence lb,Subsequence lregion,string e,Subsequence rb) {
    string res;
    append(res, '(');
    append(res, '.', size(lregion));
    append(res, e);
    append(res, ')');
    return res;
  }

  string br(Subsequence lb,string e,Subsequence rregion,Subsequence rb) {
    string res;
    append(res, '(');
    append(res, e);
    append(res, '.', size(rregion));
    append(res, ')');
    return res;
  }

  string il(Subsequence lb,Subsequence lregion,string e,Subsequence rregion,Subsequence rb) {
    string res;
    append(res, '(');
    append(res, '.', size(lregion));
    append(res, e);
    append(res, '.', size(rregion));
    append(res, ')');
    return res;
  }

  string ml(Subsequence lb, string e, Subsequence rb) {
    string res;
    append(res, '(');
    append(res, e);
    append(res, ')');
    return res;
  }

  string addss(string e,Subsequence rb) {
    string res;
    append(res, e);
    append(res, '.', size(rb));
    return res;
  }

  string incl(string e) {
    return e;
  }

  choice [string] h([string] i) {
    return i;
  }
}

grammar gra_nodangle uses sig_foldrna(axiom = struct) {
  struct    = sadd(BASE with unpaired, struct)     |
              cadd(incl(dangle), struct)   |
              nil(LOC)               # h;

  dangle    = drem(LOC, hairpin, LOC) # h;

  hairpin   = hl(BASE,                                        REGION with minsize(3) with unpaired,          BASE) with basepair # h;
}

instance mfe = gra_nodangle(alg_mfe);

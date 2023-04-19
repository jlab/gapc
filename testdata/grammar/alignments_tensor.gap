input <1DtensorF32, 1DtensorF32>
type Rope = extern

signature sig_alignments(alphabet, answer) {
  answer Ins(<alphabet, void>, <tensorslice, void>, answer);
  answer Del(<void, alphabet>, <void, tensorslice>, answer);
  answer Ers(<alphabet, alphabet>, <tensorslice, tensorslice>, answer);
  answer Sto(<void, void>);
	
  answer Region(<Rope, void>, answer, <Rope, void>);
  answer Region_Pr(<Rope, void>, answer, <void, Rope>);
  answer Region_Pr_Pr(<void, Rope>, answer, <void, Rope>);
	
  answer Insx(<alphabet, void>, <tensorslice, void>, answer);
  answer Delx(<void, alphabet>, <void, tensorslice>, answer);

  choice [answer] h([answer]);
}

algebra alg_enum auto enum;
algebra alg_count auto count;

algebra alg_hessian implements sig_alignments(alphabet=tensorchar, answer=float) {
  float Ins(<alphabet a, void>, <tensorslice locA, void>, float x) {
    return x + -2.0;
  }
  float Del(<void, alphabet b>, <void, tensorslice locB>, float x) {
    return x + -2.0;
  }
  float Ers(<alphabet a, alphabet b>, <tensorslice locA, tensorslice locB>, float x) {
    if (a == b) {
      return x +2.0;
    } else {
      return x +1.0;
    }
  }
  float Sto(<void, void>) {
    return 0.0;
  }

  float Region(<Rope aleft, void>, float x, <Rope aright, void>) {
    return x;
  }
  float Region_Pr(<Rope aleft, void>, float x, <void, Rope bright>) {
    return x;
  }
  float Region_Pr_Pr(<void, Rope bleft>, float x, <void, Rope bright>) {
    return x;
  }

  // this is slightly different form http://rna.informatik.uni-freiburg.de/Teaching/index.jsp?toolName=Gotoh#
  // as there Ins + Insx is computed for first blank, we here score Ins for first blank and Insx for all following ones
  float Insx(<alphabet a, void>, <tensorslice locA, void>, float x) {
    return x + -1.0;
  }
  float Delx(<void, alphabet b>, <void, tensorslice locB>, float x) {
    return x + -1.0;
  }

  choice [float] h([float] candidates) {
    return list(sum(candidates));
  }
}


algebra alg_score implements sig_alignments(alphabet=tensorchar, answer=float) {
  float normalize_derivative(float q, float pfunc) {
    return q / pfunc;
  }
  float Ins(<alphabet a, void>, <tensorslice locA, void>, float x) {
    return x * exp(-2.0);
  }
  float Del(<void, alphabet b>, <void, tensorslice locB>, float x) {
    return x * exp(-2.0);
  }
  float Ers(<alphabet a, alphabet b>, <tensorslice locA, tensorslice locB>, float x) {
    if (equal(a, b)) {
      return x * exp(2.0);
    } else {
      return x * exp(1.0);
    }
  }
  float Sto(<void, void>) {
    return exp(0.0);
  }
	
  float Region(<Rope aleft, void>, float x, <Rope aright, void>) {
    return x;
  }
  float Region_Pr(<Rope aleft, void>, float x, <void, Rope bright>) {
    return x;
  }
  float Region_Pr_Pr(<void, Rope bleft>, float x, <void, Rope bright>) {
    return x;
  }
	
  // this is slightly different form http://rna.informatik.uni-freiburg.de/Teaching/index.jsp?toolName=Gotoh#
  // as there Ins + Insx is computed for first blank, we here score Ins for first blank and Insx for all following ones
  float Insx(<alphabet a, void>, <tensorslice locA, void>, float x) {
    return x * exp(-1.0);
  }
  float Delx(<void, alphabet b>, <void, tensorslice locB>, float x) {
    return x * exp(-1.0);
  }

  choice [float] h([float] candidates) {
    return list(sum(candidates));
  }
}

// pair-wise global alignment with affine gap costs
grammar gra_gotoh uses sig_alignments(axiom=A) {
  A = Ins(<TCHAR, EMPTY>, <TLOC, EMPTY>, xIns)
    | Del(<EMPTY, TCHAR>, <EMPTY, TLOC>, xDel)
    | Ers(<TCHAR, TCHAR>, <TLOC, TLOC>, A)
    | Sto(<EMPTY, EMPTY>)
    # h;

  xIns = Insx(<TCHAR, EMPTY>, <TLOC, EMPTY>, xIns)
       | A
       # h;

  xDel = Delx(<EMPTY, TCHAR>, <EMPTY, TLOC>, xDel)
       | A
       # h;
}

// pair-wise global alignment
grammar gra_needlemanwunsch uses sig_alignments(axiom=A) {
  A = Ins(<TCHAR, EMPTY>, <TLOC, EMPTY>, A)
    | Del(<EMPTY, TCHAR>, <EMPTY, TLOC>, A)
    | Ers(<TCHAR, TCHAR>, <TLOC, TLOC>, A)
    | Sto(<EMPTY, EMPTY>)
    # h;
}

instance firstD_nw = gra_needlemanwunsch(alg_score);
instance firstD_gotoh = gra_gotoh(alg_score);


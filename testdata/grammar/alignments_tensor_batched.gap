input <batchedtensorF32, batchedtensorF32>
type Rope = extern

signature sig_alignments(alphabet, answer) {
  answer Ins(<alphabet, void>, <tensorslice, tensorslice>, answer);
  answer Del(<void, alphabet>, <tensorslice, tensorslice>, answer);
  answer Ers(<alphabet, alphabet>, <tensorslice, tensorslice>, answer);
  answer Sto(<void, void>);
	
  answer Region(<Rope, void>, answer, <Rope, void>);
  answer Region_Pr(<Rope, void>, answer, <void, Rope>);
  answer Region_Pr_Pr(<void, Rope>, answer, <void, Rope>);
	
  answer Insx(<alphabet, void>, <tensorslice, tensorslice>, answer);
  answer Delx(<void, alphabet>, <tensorslice, tensorslice>, answer);

  choice [answer] h([answer]);
}

algebra alg_enum auto enum;
algebra alg_count auto count;

algebra alg_hessian implements sig_alignments(alphabet=tensorchar, answer=F64batch) {
  F64batch Ins(<alphabet a, void>, <tensorslice locA, tensorslice locB>, F64batch x) {
    return x - 2.0;
  }
  F64batch Del(<void, alphabet b>, <tensorslice locA, tensorslice locB>, F64batch x) {
    return x - 2.0;
  }
  F64batch Ers(<alphabet a, alphabet b>, <tensorslice locA, tensorslice locB>, F64batch x) {
    return batched_add_if_else(a, b, x, 2.0, 1.0);
  }
  F64batch Sto(<void, void>) {
    return 0.0;
  }

  F64batch Region(<Rope aleft, void>, F64batch x, <Rope aright, void>) {
    return x;
  }
  F64batch Region_Pr(<Rope aleft, void>, F64batch x, <void, Rope bright>) {
    return x;
  }
  F64batch Region_Pr_Pr(<void, Rope bleft>, F64batch x, <void, Rope bright>) {
    return x;
  }

  // this is slightly different form http://rna.informatik.uni-freiburg.de/Teaching/index.jsp?toolName=Gotoh#
  // as there Ins + Insx is computed for first blank, we here score Ins for first blank and Insx for all following ones
  F64batch Insx(<alphabet a, void>, <tensorslice locA, tensorslice locB>, F64batch x) {
    return x - 1.0;
  }
  F64batch Delx(<void, alphabet b>, <tensorslice locA, tensorslice locB>, F64batch x) {
    return x - 1.0;
  }

  choice [F64batch] h([F64batch] candidates) {
    return list(sum(candidates));
  }
}

algebra alg_score_batched implements sig_alignments(alphabet=tensorchar, answer=F64batch) {
  F64batch normalize_derivative(F64batch q, F64batch pfunc) {
    return q / pfunc;
  }
  F64batch Ins(<alphabet a, void>, <tensorslice locA, tensorslice locB>, F64batch x) {
    return x * exp(-2.0);
  }
  F64batch Del(<void, alphabet b>, <tensorslice locA, tensorslice locB>, F64batch x) {
    return x * exp(-2.0);
  }
  F64batch Ers(<alphabet a, alphabet b>, <tensorslice locA, tensorslice locB>, F64batch x) {
    return batched_multiply_if_else(a, b, x, exp(2.0), exp(1.0));
  }
  F64batch Sto(<void, void>) {
    return exp(0.0);
  }
	
  F64batch Region(<Rope aleft, void>, F64batch x, <Rope aright, void>) {
    return x;
  }
  F64batch Region_Pr(<Rope aleft, void>, F64batch x, <void, Rope bright>) {
    return x;
  }
  F64batch Region_Pr_Pr(<void, Rope bleft>, F64batch x, <void, Rope bright>) {
    return x;
  }
	
  // this is slightly different form http://rna.informatik.uni-freiburg.de/Teaching/index.jsp?toolName=Gotoh#
  // as there Ins + Insx is computed for first blank, we here score Ins for first blank and Insx for all following ones
  F64batch Insx(<alphabet a, void>, <tensorslice locA, tensorslice locB>, F64batch x) {
    return x * exp(-1.0);
  }
  F64batch Delx(<void, alphabet b>, <tensorslice locA, tensorslice locB>, F64batch x) {
    return x * exp(-1.0);
  }

  choice [F64batch] h([F64batch] candidates) {
    return list(sum(candidates));
  }
}

algebra alg_jacobian extends alg_score_batched {
  choice [F64batch] h([F64batch] candidates) {
    return list(sum(candidates));
  }
}


// pair-wise global alignment with affine gap costs
grammar gra_gotoh uses sig_alignments(axiom=A) {
  A = Ins(<TCHAR, EMPTY>, <TLOC, TLOC>, xIns)
    | Del(<EMPTY, TCHAR>, <TLOC, TLOC>, xDel)
    | Ers(<TCHAR, TCHAR>, <TLOC, TLOC>, A)
    | Sto(<EMPTY, EMPTY>)
    # h;

  xIns = Insx(<TCHAR, EMPTY>, <TLOC, TLOC>, xIns)
       | A
       # h;

  xDel = Delx(<EMPTY, TCHAR>, <TLOC, TLOC>, xDel)
       | A
       # h;
}

// pair-wise global alignment
grammar gra_needlemanwunsch uses sig_alignments(axiom=A) {
  A = Ins(<TCHAR, EMPTY>, <TLOC, TLOC>, A)
    | Del(<EMPTY, TCHAR>, <TLOC, TLOC>, A)
    | Ers(<TCHAR, TCHAR>, <TLOC, TLOC>, A)
    | Sto(<EMPTY, EMPTY>)
    # h;
}

instance nw_batched_deriv_1 = gra_needlemanwunsch(alg_score_batched);
instance gotoh_batched_deriv_1 = gra_gotoh(alg_score_batched);

instance nw_batched_deriv_2 = gra_needlemanwunsch(alg_jacobian * alg_hessian);
instance gotoh_batched_deriv_2 = gra_gotoh(alg_jacobian * alg_hessian);

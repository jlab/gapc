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

algebra alg_hessian implements sig_alignments(alphabet=tensorchar, answer=F32batch) {
  F32batch Ins(<alphabet a, void>, <tensorslice locA, tensorslice locB>, F32batch x) {
    return x + -2.0;
  }
  F32batch Del(<void, alphabet b>, <tensorslice locA, tensorslice locB>, F32batch x) {
    return x + -2.0;
  }
  F32batch Ers(<alphabet a, alphabet b>, <tensorslice locA, tensorslice locB>, F32batch x) {
    return batched_add_if_else(a, b, x, 2.0, 1.0);
  }
  F32batch Sto(<void, void>) {
    return 0.0;
  }

  F32batch Region(<Rope aleft, void>, F32batch x, <Rope aright, void>) {
    return x;
  }
  F32batch Region_Pr(<Rope aleft, void>, F32batch x, <void, Rope bright>) {
    return x;
  }
  F32batch Region_Pr_Pr(<void, Rope bleft>, F32batch x, <void, Rope bright>) {
    return x;
  }

  // this is slightly different form http://rna.informatik.uni-freiburg.de/Teaching/index.jsp?toolName=Gotoh#
  // as there Ins + Insx is computed for first blank, we here score Ins for first blank and Insx for all following ones
  F32batch Insx(<alphabet a, void>, <tensorslice locA, tensorslice locB>, F32batch x) {
    return x + -1.0;
  }
  F32batch Delx(<void, alphabet b>, <tensorslice locA, tensorslice locB>, F32batch x) {
    return x + -1.0;
  }

  choice [F32batch] h([F32batch] candidates) {
    return list(sum(candidates));
  }
}

algebra alg_score_batched implements sig_alignments(alphabet=tensorchar, answer=F32batch) {
  F32batch normalize_derivative(F32batch q, F32batch pfunc) {
    return q / pfunc;
  }
  F32batch Ins(<alphabet a, void>, <tensorslice locA, tensorslice locB>, F32batch x) {
    return x * exp(-2.0);
  }
  F32batch Del(<void, alphabet b>, <tensorslice locA, tensorslice locB>, F32batch x) {
    return x * exp(-2.0);
  }
  F32batch Ers(<alphabet a, alphabet b>, <tensorslice locA, tensorslice locB>, F32batch x) {
    return batched_multiply_if_else(a, b, x, exp(2.0), exp(1.0));
  }
  F32batch Sto(<void, void>) {
    return exp(0.0);
  }
	
  F32batch Region(<Rope aleft, void>, F32batch x, <Rope aright, void>) {
    return x;
  }
  F32batch Region_Pr(<Rope aleft, void>, F32batch x, <void, Rope bright>) {
    return x;
  }
  F32batch Region_Pr_Pr(<void, Rope bleft>, F32batch x, <void, Rope bright>) {
    return x;
  }
	
  // this is slightly different form http://rna.informatik.uni-freiburg.de/Teaching/index.jsp?toolName=Gotoh#
  // as there Ins + Insx is computed for first blank, we here score Ins for first blank and Insx for all following ones
  F32batch Insx(<alphabet a, void>, <tensorslice locA, tensorslice locB>, F32batch x) {
    return x * exp(-1.0);
  }
  F32batch Delx(<void, alphabet b>, <tensorslice locA, tensorslice locB>, F32batch x) {
    return x * exp(-1.0);
  }

  choice [F32batch] h([F32batch] candidates) {
    return list(sum(candidates));
  }
}

algebra alg_jacobian extends alg_score_batched {
  choice [F32batch] h([F32batch] candidates) {
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

instance firstD_nw_batched = gra_needlemanwunsch(alg_score_batched);
instance firstD_gotoh_batched = gra_gotoh(alg_score_batched);

instance bothD_nw_batched = gra_needlemanwunsch(alg_jacobian * alg_hessian);
instance bothD_gotoh_batched = gra_gotoh(alg_jacobian * alg_hessian);

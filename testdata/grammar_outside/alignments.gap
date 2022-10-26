input <raw, raw>
type Rope = extern

signature sig_alignments(alphabet, answer) {
  answer Ins(<alphabet, void>, <Subsequence, void>, answer);
  answer Del(<void, alphabet>, <void, Subsequence>, answer);
  answer Ers(<alphabet, alphabet>, <Subsequence, Subsequence>, answer);
  answer Sto(<void, void>);
	
  answer Region(<Rope, void>, answer, <Rope, void>);
  answer Region_Pr(<Rope, void>, answer, <void, Rope>);
  answer Region_Pr_Pr(<void, Rope>, answer, <void, Rope>);
	
  answer Insx(<alphabet, void>, <Subsequence, void>, answer);
  answer Delx(<void, alphabet>, <void, Subsequence>, answer);

  choice [answer] h([answer]);
}

algebra alg_enum auto enum;
algebra alg_count auto count;

algebra alg_similarity implements sig_alignments(alphabet=char, answer=int) {
  int Ins(<alphabet a, void>, <Subsequence locA, void>, int x) {
    return x -3;
  }
  int Del(<void, alphabet b>, <void, Subsequence locB>, int x) {
    return x -2;
  }
  int Ers(<alphabet a, alphabet b>, <Subsequence locA, Subsequence locB>, int x) {
    if (a == b) {
      return x +1;
    } else {
      return x -1;
    }
  }
  int Sto(<void, void>) {
    return 0;
  }
	
  int Region(<Rope aleft, void>, int x, <Rope aright, void>) {
    return x;
  }
  int Region_Pr(<Rope aleft, void>, int x, <void, Rope bright>) {
    return x;
  }
  int Region_Pr_Pr(<void, Rope bleft>, int x, <void, Rope bright>) {
    return x;
  }
	
  // this is slightly different form http://rna.informatik.uni-freiburg.de/Teaching/index.jsp?toolName=Gotoh#
  // as there Ins + Insx is computed for first blank, we here score Ins for first blank and Insx for all following ones
  int Insx(<alphabet a, void>, <Subsequence locA, void>, int x) {
    return x -1;
  }
  int Delx(<void, alphabet b>, <void, Subsequence locB>, int x) {
    return x -1;
  }

  choice [int] h([int] candidates) {
    return list(maximum(candidates));
  }
}

algebra alg_score implements sig_alignments(alphabet=char, answer=float) {
  float Ins(<alphabet a, void>, <Subsequence locA, void>, float x) {
    return x * exp(-2.0);
  }
  float Del(<void, alphabet b>, <void, Subsequence locB>, float x) {
    return x * exp(-2.0);
  }
  float Ers(<alphabet a, alphabet b>, <Subsequence locA, Subsequence locB>, float x) {
    if (a == b) {
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
  float Insx(<alphabet a, void>, <Subsequence locA, void>, float x) {
    return x * exp(-1.0);
  }
  float Delx(<void, alphabet b>, <void, Subsequence locB>, float x) {
    return x * exp(-1.0);
  }

  choice [float] h([float] candidates) {
    return list(sum(candidates));
  }
}


algebra alg_countmanual implements sig_alignments(alphabet=char, answer=int) {
  int Ins(<alphabet a, void>, <Subsequence locA, void>, int x) {
    return x;
  }
  int Del(<void, alphabet b>, <void, Subsequence locB>, int x) {
    return x;
  }
  int Ers(<alphabet a, alphabet b>, <Subsequence locA, Subsequence locB>, int x) {
    return x;
  }
  int Sto(<void, void>) {
    return 1;
  }
	
  int Region(<Rope aleft, void>, int x, <Rope aright, void>) {
    return x;
  }
  int Region_Pr(<Rope aleft, void>, int x, <void, Rope bright>) {
    return x;
  }
  int Region_Pr_Pr(<void, Rope bleft>, int x, <void, Rope bright>) {
    return x;
  }
	
  int Insx(<alphabet a, void>, <Subsequence locA, void>, int x) {
    return x;
  }
  int Delx(<void, alphabet b>, <void, Subsequence locB>, int x) {
    return x;
  }

  choice [int] h([int] candidates) {
    return list(sum(candidates));
  }
}


// pair-wise global alignment
grammar gra_needlemanwunsch uses sig_alignments(axiom=A) {
  A = Ins(<CHAR, EMPTY>, <LOC, EMPTY>, A)
    | Del(<EMPTY, CHAR>, <EMPTY, LOC>, A)
    | Ers(<CHAR, CHAR>, <LOC, LOC>, A)
    | Sto(<EMPTY, EMPTY>)
    # h;
}

// pair-wise global alignment with affine gap costs
grammar gra_gotoh uses sig_alignments(axiom=A) {
  A = Ins(<CHAR, EMPTY>, <LOC, EMPTY>, xIns)
    | Del(<EMPTY, CHAR>, <EMPTY, LOC>, xDel)
    | Ers(<CHAR, CHAR>, <LOC, LOC>, A)
    | Sto(<EMPTY, EMPTY>)
    # h;

  xIns = Insx(<CHAR, EMPTY>, <LOC, EMPTY>, xIns)
       | A
       # h;

  xDel = Delx(<EMPTY, CHAR>, <EMPTY, LOC>, xDel)
       | A
       # h;
}

instance count = gra_needlemanwunsch(alg_count);
instance sim_enum = gra_needlemanwunsch(alg_similarity * alg_enum);
instance firstD = gra_needlemanwunsch(alg_score);
instance firstD_gotoh = gra_gotoh(alg_score);

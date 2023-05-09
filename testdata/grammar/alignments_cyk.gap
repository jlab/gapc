input <raw, raw>
type Rope = extern

signature sig_alignments(alphabet, answer) {
  answer Ins(<alphabet, void>, <Subsequence, void>, answer);
  answer Del(<void, alphabet>, <void, Subsequence>, answer);
  answer Ers(<alphabet, alphabet>, <Subsequence, Subsequence>, answer);
  answer Sto(<void, void>);
	
  choice [answer] h([answer]);
        
  answer split(<Rope, Rope>, answer);
  answer splitR(answer, <Rope, Rope>);
}

algebra alg_enum auto enum;
algebra alg_count auto count;



// pair-wise global alignment
grammar gra_NWcyk uses sig_alignments(axiom=nt_const) {
  nt_const = nt_left # h;
        
  nt_left = split(<ROPE, ROPE>, nt_right) # h;
  nt_right = splitR(A, <ROPE, ROPE>) # h;
        
  A = Ins(<CHAR, EMPTY>, <LOC, EMPTY>, A)
    | Del(<EMPTY, CHAR>, <EMPTY, LOC>, A)
    | Ers(<CHAR, CHAR>, <LOC, LOC>, A)
    | Sto(<EMPTY, EMPTY>)
    # h;
}


instance count = gra_NWcyk(alg_count);

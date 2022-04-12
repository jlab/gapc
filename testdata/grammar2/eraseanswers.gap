import rna

input rna

signature sig(alphabet,answer) {
	answer foo(Subsequence,Subsequence);
	answer bar(Subsequence,Subsequence);
	choice [answer] h([answer]);
}

algebra alg_enum auto enum;
algebra alg_count auto count;

grammar gra_working uses sig(axiom = start) {
  start = foo(REGION, REGION) with basepairing
        | bar(REGION, REGION)
        # h;        
}

grammar gra_failing uses sig(axiom = start) {
  start = bar(REGION, REGION)
        | foo(REGION, REGION) with basepairing
        # h;        
}

instance ins_enum = gra_working(alg_enum);
instance ins_count_working = gra_working(alg_count);
instance ins_count_failing = gra_failing(alg_count);

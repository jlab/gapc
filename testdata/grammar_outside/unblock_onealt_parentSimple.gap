import rna
import "singlefold.hh"

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


grammar gra_nodangle uses sig_foldrna(axiom = struct) {
  struct    = sadd(BASE, {struct})
            | nil(LOC) 
            # h;

  dangle    = drem(LOC, weak, LOC) # h;

  weak      = {hairpin}    # h;

  hairpin   = hl(BASE,                          REGION with minsize(3),          BASE) with basepairing # h;
}


instance ins_count = gra_nodangle(alg_count);

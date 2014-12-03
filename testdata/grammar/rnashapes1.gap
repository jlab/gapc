// translated from: Grammar_canonicals_nonamb.lhs
// which is the RNAshapes grammar


import rna

input rna

signature Canonical_Algebra(alphabet, answer) {
answer sadd(Subsequence,answer);
answer cadd(answer,answer);
answer cadd_Pr(answer,answer);
answer cadd_Pr_Pr(answer,answer);
answer cadd_Pr_Pr_Pr(answer,answer);
answer ambd(answer,Subsequence,answer);
answer ambd_Pr(answer,Subsequence,answer);
answer nil(void);
answer nil_Pr(void);
answer edl(Subsequence,answer);
answer edr(answer,Subsequence);
answer edlr(Subsequence,answer,Subsequence);
answer drem(answer);
answer is(answer);
answer sr(Subsequence,answer,Subsequence);
answer hl(Subsequence,Subsequence,Subsequence,Subsequence,Subsequence);
answer hlChar(Subsequence,Subsequence,Subsequence,Subsequence,Subsequence);
answer sp(Subsequence,Subsequence,answer,Subsequence,Subsequence);
answer bl(Subsequence,answer);
answer br(answer,Subsequence);
answer il(Subsequence,answer,Subsequence);
answer ml(Subsequence,Subsequence,answer,Subsequence,Subsequence);
answer mldr(Subsequence,Subsequence,answer,Subsequence,Subsequence,Subsequence);
answer mladr(Subsequence,Subsequence,answer,Subsequence,Subsequence,Subsequence);
answer mldlr(Subsequence,Subsequence,Subsequence,answer,Subsequence,Subsequence,Subsequence);
answer mladlr(Subsequence,Subsequence,Subsequence,answer,Subsequence,Subsequence,Subsequence);
answer mldladr(Subsequence,Subsequence,Subsequence,answer,Subsequence,Subsequence,Subsequence);
answer mladldr(Subsequence,Subsequence,Subsequence,answer,Subsequence,Subsequence,Subsequence);
answer mldl(Subsequence,Subsequence,Subsequence,answer,Subsequence,Subsequence);
answer mladl(Subsequence,Subsequence,Subsequence,answer,Subsequence,Subsequence);
answer addss(answer,Subsequence);
answer ssadd(Subsequence,answer);
answer trafo(answer);
answer incl(answer);
answer combine(answer,answer);
answer lcombine(answer,answer);
answer lcombine_Pr(answer,answer);
answer rcombine(answer,answer);
answer rcombine_Pr(answer,answer);
answer lrcombine(answer,answer);
answer acomb(answer,Subsequence,answer);
answer lacomb(answer,Subsequence,answer);
answer lacomb_Pr(answer,Subsequence,answer);
answer racomb(answer,Subsequence,answer);
answer racomb_Pr(answer,Subsequence,answer);
answer lracomb(answer,Subsequence,answer);
choice [answer] h([answer]);
}

algebra count auto count ;

algebra enum auto enum ;

grammar canonicals_nonamb uses Canonical_Algebra (axiom = struct) {

  tabulated {
 noleft_dangle,
 left_dangle,
 block_dl,
 dl_or_ss_left_ss_end,
 no_dl_ss_end,
 block_dlr,
 ml_comps3,
 ml_comps1,
 ml_comps2,
 no_dl_no_ss_end,
 initstem,
 edangler,
 ml_comps4,
 nodangle,
 left_unpaired,
 edanglel,
 edanglelr,
 dl_or_ss_left_no_ss_end,
 leftB,
 rightB,
 closed
}

  struct = left_dangle |
    trafo(noleft_dangle) |
    left_unpaired # h 
;


  left_unpaired = sadd(BASE, left_unpaired) |
    sadd(BASE, left_dangle) # h 
;


  left_dangle = ambd(edanglel, BASE, noleft_dangle) |
    cadd_Pr(edanglel,  { noleft_dangle | nil_Pr(EMPTY) } ) |
    cadd(edanglelr,  { left_dangle | left_unpaired } ) |
    nil(EMPTY) # h 
;


  noleft_dangle = cadd_Pr_Pr(edangler,  { left_dangle | left_unpaired } ) |
    cadd_Pr_Pr_Pr(nodangle,  { noleft_dangle | nil_Pr(EMPTY) } ) |
    ambd_Pr(nodangle, BASE, noleft_dangle) # h 
;


  edanglel = edl(BASE, initstem) # h 
;


  edangler = edr(initstem, BASE) # h 
;


  edanglelr = edlr(BASE, initstem, BASE) # h 
;


  nodangle = drem(initstem) # h 
;


  initstem = is(closed) # h 
;


  closed = stack |
    hairpin |
    multiloop |
    leftB |
    rightB |
    iloop # h 
;


  multiloop =  { mldl(BASE, BASE, BASE, ml_comps1, BASE, BASE) |
    mladl(BASE, BASE, BASE, ml_comps2, BASE, BASE) |
    mldr(BASE, BASE, ml_comps3, BASE, BASE, BASE) |
    mladr(BASE, BASE, ml_comps2, BASE, BASE, BASE) |
    mldlr(BASE, BASE, BASE, ml_comps4, BASE, BASE, BASE) |
    mladlr(BASE, BASE, BASE, ml_comps2, BASE, BASE, BASE) |
    mldladr(BASE, BASE, BASE, ml_comps1, BASE, BASE, BASE) |
    mladldr(BASE, BASE, BASE, ml_comps3, BASE, BASE, BASE) |
    ml(BASE, BASE, ml_comps2, BASE, BASE) } with stackpairing 
;


  ml_comps1 = combine(block_dl, no_dl_no_ss_end) |
    combine(block_dlr, dl_or_ss_left_no_ss_end) |
    acomb(block_dl, BASE, no_dl_no_ss_end) # h 
;


  ml_comps2 = combine(incl(nodangle), no_dl_no_ss_end) |
    combine(incl(edangler), dl_or_ss_left_no_ss_end) |
    acomb(incl(nodangle), BASE, no_dl_no_ss_end) # h 
;


  ml_comps3 = combine(incl(edangler), dl_or_ss_left_ss_end) |
    combine(incl(nodangle), no_dl_ss_end) |
    acomb(incl(nodangle), BASE, no_dl_ss_end) # h 
;


  ml_comps4 = combine(block_dl, no_dl_ss_end) |
    combine(block_dlr, dl_or_ss_left_ss_end) |
    acomb(block_dl, BASE, no_dl_ss_end) # h 
;


  block_dl = ssadd(REGION, edanglel) |
    incl(edanglel) # h 
;


  block_dlr = ssadd(REGION, edanglelr) |
    incl(edanglelr) # h 
;


  no_dl_no_ss_end = ml_comps2 |
    incl(nodangle) # h 
;


  dl_or_ss_left_no_ss_end = ml_comps1 |
    block_dl # h 
;


  no_dl_ss_end = ml_comps3 |
    incl(edangler) |
    addss(incl(edangler), REGION) # h 
;


  dl_or_ss_left_ss_end = ml_comps4 |
    block_dlr |
    addss(block_dlr, REGION) # h 
;


  stack = sr(BASE, closed, BASE) with basepairing 
;


  hairpin = hl(BASE, BASE, REGION with minsize(3), BASE, BASE) with stackpairing 
;


  leftB = sp(BASE, BASE, bl(REGION, initstem), BASE, BASE) with stackpairing # h
;


  rightB = sp(BASE, BASE, br(initstem, REGION), BASE, BASE) with stackpairing # h
;


  iloop = sp(BASE, BASE, il(REGION with maxsize(30), closed, REGION with maxsize(30)), BASE, BASE) with stackpairing
   #h
;



}



instance count = canonicals_nonamb ( count ) ;
instance enu = canonicals_nonamb ( enum ) ;



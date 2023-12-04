import rna

input rna

signature FS_Algebra(alphabet,comp) {
comp sadd(Subsequence,comp);
comp cadd(comp,comp);
comp mlcons(comp,comp);
comp dlr(Subsequence,comp,Subsequence);
comp sr(Subsequence,comp,Subsequence);
comp hl(Subsequence,Subsequence,Subsequence);
comp bl(Subsequence,Subsequence,comp,Subsequence);
comp br(Subsequence,comp,Subsequence,Subsequence);
comp il(Subsequence,Subsequence,comp,Subsequence,Subsequence);
comp ml(Subsequence,comp,Subsequence);
//comp append(comp,comp);
comp ul(comp);
comp addss(comp,Subsequence);
comp addssml(comp,Subsequence);
comp ssadd(Subsequence,comp);
comp ss(Subsequence);
comp nil(Subsequence);
//comp pk(TInt,Subsequence,comp,Subsequence,comp,Subsequence,comp,Subsequence);
comp pul(comp);
comp pss(Subsequence);
//blah sum(Subsequence,blah,Subsequence);
//blah sumend(Subsequence,Subsequence,Subsequence);
choice [comp] h([comp]);
}

algebra count auto count ;

algebra enum auto enum ;

grammar tdm uses FS_Algebra (axiom = rnastruct) {

  tabulated {
     tail2,
     tail1,
     tail0,
     structstart,
     rnastruct,
     motif0,
     motif3,
     motif2,
     motif5,
     tail3
  }

  rnastruct = sadd(BASE, rnastruct) |
    addss(structstart, UREGION) # h 
;


  structstart = cadd(motif0, tail0) # h 
;


  motif0 = dlr(LOC, stem0, LOC) # h 
;


  stem0 = sr(BASE, sr(BASE, sr(BASE, maxstem0, BASE) with basepairing, BASE) with basepairing, BASE) with basepairing # h 
;


  maxstem0 = sr(BASE,  { motif1 | sr(BASE,  { motif1 | sr(BASE,  { motif1 | sr(BASE,  { motif1 | sr(BASE,  { motif1 | sr(BASE,  { motif1 | sr(BASE,  { motif1 | sr(BASE,  { motif1 | sr(BASE,  { motif1 | sr(BASE,  { motif1 | sr(BASE, motif1, BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing # h 
;


  motif1 = hairpin1 # h 
;


  hairpin1 = hl(BASE, REGION with minsize(3), BASE) with basepairing # h 
;


  tail0 = cadd(motif2, tail1) # h 
;


  motif2 = ss(UREGION) # h 
;


  tail1 = cadd(motif3, tail2) # h 
;


  motif3 = dlr(LOC, stem3, LOC) # h 
;


  stem3 = sr(BASE, maxstem3, BASE) with basepairing # h 
;


  maxstem3 = sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE,  { motif4 | sr(BASE, motif4, BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing # h 
;


  motif4 = hairpin4 # h 
;


  hairpin4 = hl(BASE, REGION with minsize(3), BASE) with basepairing # h 
;


  tail2 = cadd(motif5, tail3) # h 
;


  motif5 = ss(UREGION) # h 
;


  tail3 = motif6 
;


  motif6 = dlr(LOC, stem6, LOC) # h 
;


  stem6 = sr(BASE, sr(BASE, maxstem6, BASE) with basepairing, BASE) with basepairing # h 
;


  maxstem6 = sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE,  { motif7 | sr(BASE, motif7, BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing } , BASE) with basepairing # h 
;


  motif7 = hairpin7 # h 
;


  hairpin7 = hl(BASE, REGION with minsize(3), BASE) with basepairing # h 
;



}

instance count = tdm ( count ) ;
instance enu = tdm ( enum ) ;

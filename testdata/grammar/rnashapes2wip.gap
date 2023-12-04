import rna

input rna

type mfeanswer = (int energy, Subsequence leftBase, Subsequence rightBase, string rep)

/*signature Foo(alphabet, answer) {
  choice [answer] h([answer]);
}
*/

signature Canonical_Algebra(alphabet,answer)
{
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
  answer acomb(answer,Subsequence,answer);
choice [answer] h([answer]);
}

algebra count auto count ;

algebra enum auto enum ;

algebra mfe implements Canonical_Algebra(alphabet = char, answer = mfeanswer)
{
  mfeanswer sadd(Subsequence lb,mfeanswer e) {
    mfeanswer res;
    res.energy = e.energy;
    res.leftBase = lb;
    res.rightBase = e.rightBase;
    	  string o;
	  append(o, "sadd{", 5);
	  append(o, e.rep);
	  append(o, "}",1);
	  res.rep = o;
    return res;
  }

  mfeanswer cadd(mfeanswer le,mfeanswer re) {
    mfeanswer res;
    res.energy = le.energy + re.energy;
    res.leftBase = le.leftBase;
    res.rightBase = le.rightBase;
          string o;
	  append(o, "cadd{", 5);
	  append(o, le.rep);
	  append(o, ",", 1);
	  append(o, re.rep);
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer cadd_Pr(mfeanswer le,mfeanswer re) {
    mfeanswer res;
    res.energy = le.energy + re.energy;
    res.leftBase = le.leftBase;
    res.rightBase = le.rightBase;
    	  string o;
	  append(o, "cadd'{", 6);
	  append(o, le.rep);
	  append(o, ",", 1);
	  append(o, re.rep);
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer cadd_Pr_Pr(mfeanswer le,mfeanswer re) {
    mfeanswer res;
    res.energy = le.energy + re.energy;
    res.leftBase = le.leftBase;
    res.rightBase = le.rightBase;
    	  string o;
	  append(o, "cadd''{", 7);
	  append(o, le.rep);
	  append(o, ",", 1);
	  append(o, re.rep);
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer cadd_Pr_Pr_Pr(mfeanswer le,mfeanswer re) {
    mfeanswer res;
    res.energy = le.energy + re.energy;
    res.leftBase = le.leftBase;
    res.rightBase = le.rightBase;
    	  string o;
	  append(o, "cadd'''{", 8);
	  append(o, le.rep);
	  append(o, ",", 1);
	  append(o, re.rep);
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer ambd(mfeanswer le,Subsequence b,mfeanswer re) {
    mfeanswer res;
    res.energy = le.energy + re.energy + min(dr_energy(le.leftBase, le.rightBase), dl_energy(re.leftBase, re.rightBase));
    res.leftBase = le.leftBase;
    res.rightBase = le.rightBase;
	  string o1;
	  string o2;
	  append(o1, "ambd{", 5);
	  append(o1, le.rep);
	  append(o1, ",", 1);
	  append(o1, re.rep);
	  append(o1, ",min(dr_energy(", 15);
	  append(o2, dr_energy(le.leftBase, le.rightBase));
	  append(o2, "),dl_energy(", 12);
	  append(o2, ")=",2);
	  append(o2, min(dr_energy(le.leftBase, le.rightBase), dl_energy(re.leftBase, re.rightBase)));
	  append(o2, ")}", 2);
	  string o;
	  append(o,o1);
	  append(o,o2);
	  res.rep = o;
    return res;
  }

  mfeanswer ambd_Pr(mfeanswer le,Subsequence b,mfeanswer re) {
    mfeanswer res;
    res.energy = le.energy + re.energy + min(dr_energy(le.leftBase, le.rightBase), dl_energy(re.leftBase, re.rightBase));
    res.leftBase = le.leftBase;
    res.rightBase = le.rightBase;
	  string o1;
	  string o2;
	  append(o1, "ambd'{", 5);
	  append(o1, le.rep);
	  append(o1, ",", 1);
	  append(o1, re.rep);
	  append(o1, ",min(dr_energy(", 15);
	  append(o2, dr_energy(le.leftBase, le.rightBase));
	  append(o2, "),dl_energy(", 12);
	  append(o2, ")=",2);
	  append(o2, min(dr_energy(le.leftBase, le.rightBase), dl_energy(re.leftBase, re.rightBase)));
	  append(o2, ")}", 2);
	  string o;
	  append(o,o1);
	  append(o,o2);
	  res.rep = o;
    return res;
  }

  mfeanswer nil(void) {
    mfeanswer res;
    res.energy = 0;
	  string o;
	  append(o, "nil{", 4);
	  append(o, "0}", 2);
	  res.rep = o;
    return res;
  }

  mfeanswer nil_Pr(void) {
    mfeanswer res;
    res.energy = 0;
	  string o;
	  append(o, "nil'{", 5);
	  append(o, "0}", 2);
	  res.rep = o;
    return res;
  }

  mfeanswer edl(Subsequence lb,mfeanswer e) {
    mfeanswer res;
    res.energy = e.energy + dl_energy(e.leftBase, e.rightBase);
    res.leftBase = e.leftBase;
    res.rightBase = e.rightBase;
	  string o;
	  append(o, "edl{", 4);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, dl_energy(e.leftBase, e.rightBase));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer edr(mfeanswer e,Subsequence rb) {
    mfeanswer res;
    res.energy = e.energy + dr_energy(e.leftBase, e.rightBase);
    res.leftBase = e.leftBase;
    res.rightBase = e.rightBase;
	  string o;
	  append(o, "edr{", 4);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, dr_energy(e.leftBase, e.rightBase));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer edlr(Subsequence lb,mfeanswer e,Subsequence rb) {
    mfeanswer res;
    res.energy = e.energy + dl_energy(e.leftBase, e.rightBase) + dr_energy(e.leftBase, e.rightBase);
    res.leftBase = e.leftBase;
    res.rightBase = e.rightBase;
	  string o;
	  append(o, "edlr{", 5);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, ext_mismatch_energy(e.leftBase, e.rightBase));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer drem(mfeanswer e) {
    mfeanswer res;
    res = e;
    res.leftBase = e.leftBase;
    res.rightBase = e.rightBase;
	  string o;
	  append(o, "drem{", 5);
	  append(o, e.rep);
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer is(mfeanswer e) {
    mfeanswer res;
    res.energy = e.energy + termau_energy(e.leftBase, e.rightBase);
    res.leftBase = e.leftBase;
    res.rightBase = e.rightBase;
	  string o;
	  append(o, "is{", 3);
	  append(o, e.rep);
	  append(o, ",termaupenalty(",15);
	  append(o, termau_energy(e.leftBase, e.rightBase));
	  append(o, ")}", 2);
	  res.rep = o;
    return res;
  }

  mfeanswer sr(Subsequence lb,mfeanswer e,Subsequence rb) {
    mfeanswer res;
    res.energy = e.energy + sr_energy(lb,rb);
    res.leftBase = lb;
    res.rightBase = rb;
	  string o;
	  append(o, "sr{", 3);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, sr_energy(lb,rb));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer hl(Subsequence llb,Subsequence lb,Subsequence region,Subsequence rb,Subsequence rrb) {
    mfeanswer res;
    res.energy = hl_energy(region) + sr_energy(llb,rrb);
    res.leftBase = llb;
    res.rightBase = rrb;
          string o;
	  append(o, "hl{", 3);
	  append(o, hl_energy(region) + sr_energy(llb,rrb));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer sp(Subsequence llb,Subsequence lb,mfeanswer e,Subsequence rb,Subsequence rrb) {
    mfeanswer res;
    res.energy = e.energy + sr_energy(llb,rrb);
    res.leftBase = llb;
    res.rightBase = rrb;
	  string o;
	  append(o, "sp{", 3);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, sr_energy(llb,rrb));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer bl(Subsequence lb,mfeanswer e) {
    mfeanswer res;
   	  Subsequence h1;
   	  Subsequence h3;
	  h1 = lb;
	  h3 = lb;
	  h1.i = lb.i-1;
	  h3.j = e.rightBase.j+1;
	  res.energy = e.energy + bl_energy(lb,h3);
	  
    res.leftBase.i = lb.i;
    res.leftBase.j = res.leftBase.i+1;
    res.leftBase.seq = lb.seq;
    res.rightBase = e.rightBase;
	  string o;
	  append(o, "bl{", 3);
	  append(o, lb);
	  append(o, e.rep);
	  append(o, ",", 1);
	  //~ append(o, bl_energy(lb, lb, helpr));
	  append(o, bl_energy(lb,e.rightBase));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer br(mfeanswer e,Subsequence rb) {
    mfeanswer res;
    Subsequence helpr;
    helpr.i = rb.j;
    helpr.j = helpr.i+1;
    helpr.seq = rb.seq;
    
	  Subsequence h1;
	  Subsequence h3;
	  h1 = rb;
	  h3 = rb;
	  h1.i = e.leftBase.i-1;
	  h3.j = rb.j+1;
	  
    res.energy = e.energy + br_energy(h1, rb);  
    res.leftBase = e.leftBase;
    res.rightBase.j = rb.j;
    res.rightBase.i = res.rightBase.j-1;
    res.rightBase.seq = res.leftBase.seq;
	  string o;
	  append(o, "br{", 3);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, br_energy(rb, h3));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer il(Subsequence lregion,mfeanswer e,Subsequence rregion) {
    mfeanswer res;
    res.energy = e.energy + il_energy(lregion, rregion);
    res.leftBase.i = lregion.i;
    res.leftBase.j = res.leftBase.i+1;
    res.leftBase.seq = lregion.seq;
    res.rightBase.j = rregion.j;
    res.rightBase.i = res.rightBase.j-1;
    res.rightBase.seq = res.leftBase.seq;
	  string o;
	  append(o, "il{", 3);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, il_energy(lregion, rregion));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer ml(Subsequence llb,Subsequence lb,mfeanswer e,Subsequence rb,Subsequence rrb) {
    mfeanswer res;
    res.energy = ml_energy() + ul_energy() + e.energy + sr_energy(llb,rrb) + termau_energy(lb,rb);
    res.leftBase = llb;
    res.rightBase = rrb;
	  string o;
	  append(o, "ml{", 3);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, ml_energy() + ul_energy()+sr_energy(llb,rrb) + termau_energy(lb,rb));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer mldr(Subsequence llb,Subsequence lb,mfeanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
    mfeanswer res;
    res.energy = ml_energy() + ul_energy() + e.energy + dri_energy(lb,rb) + sr_energy(llb,rrb) + termau_energy(lb,rb);
    res.leftBase = llb;
    res.rightBase = rrb;
	  string o;
	  append(o, "mldr{", 5);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, ml_energy() + ul_energy()+dri_energy(lb,rb) + sr_energy(llb,rrb) + termau_energy(lb,rb));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer mladr(Subsequence llb,Subsequence lb,mfeanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
    mfeanswer res;
    res.energy = ml_energy() + ul_energy() + e.energy + min(dri_energy(lb,rb), dr_energy(e.rightBase, e.rightBase)) + sr_energy(llb,rrb) + termau_energy(lb,rb);
    res.leftBase = llb;
    res.rightBase = rrb;
	  string o1;
	  string o2;
	  string o3;
	  append(o1, "mladr{380,", 10);
	  append(o1, e.rep);
	  append(o1, ",", 1);
	  append(o1, "min(dri_energy(", 15);
	  append(o1, dri_energy(lb,rb));
	  append(o2, "),dr_energy(", 12);
	  append(o2, dr_energy(e.rightBase, e.rightBase));
	  append(o2, ")=",2);
	  append(o2, min(dri_energy(lb,rb), dr_energy(e.rightBase, e.rightBase)));
          append(o3, "),sr_energy(",12);
	  append(o3, sr_energy(llb,rrb));
          append(o3, "), termaupenalty(", 16);
          append(o3, termau_energy(lb,rb));
	  append(o3, ")}", 2);
	  string o;
	  append(o,o1);
	  append(o,o2);
	  append(o,o3);
	  res.rep = o;
    return res;
  }

  mfeanswer mldlr(Subsequence llb,Subsequence lb,Subsequence dl,mfeanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
    mfeanswer res;
    res.energy = ml_energy() + ul_energy() + e.energy + ml_mismatch_energy(lb,rb) + sr_energy(llb,rrb) + termau_energy(lb,rb);
    res.leftBase = llb;
    res.rightBase = rrb;
	  string o;
	  append(o, "mldlr{", 6);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, ml_energy() + ul_energy()+ml_mismatch_energy(lb,rb) + sr_energy(llb,rrb) + termau_energy(lb,rb));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer mladlr(Subsequence llb,Subsequence lb,Subsequence dl,mfeanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
    mfeanswer res;
    res.energy = ml_energy() + ul_energy() + e.energy + min(dli_energy(lb,rb), dl_energy(e.leftBase, e.leftBase)) + min(dri_energy(lb,rb), dr_energy(e.rightBase, e.rightBase)) + sr_energy(llb,rrb) + termau_energy(lb,rb);
    res.leftBase = llb;
    res.rightBase = rrb;
	  string o1;
	  string o2;
	  string o3;
	  string o4;
	  string o5;
	  append(o1, "mladlr{380,", 11);
	  append(o1, e.rep);
	  append(o1, ",min(dli_energy(",16);
	  append(o1, dli_energy(lb,rb));
	  append(o2, "),dl_energy(",12);
	  append(o2, dl_energy(e.leftBase, e.leftBase));
	  append(o2, "=",1);
	  append(o2, min(dli_energy(lb,rb), dr_energy(e.leftBase, e.leftBase)));
	  append(o3, "),min(dri_energy(",17);
	  append(o3, dri_energy(lb,rb));
	  append(o3, "),dl_energy(",12);
	  append(o3, dr_energy(e.rightBase, e.rightBase));
	  append(o4, "=",1);
	  append(o4, min(dri_energy(lb,rb), dr_energy(e.rightBase, e.rightBase)));
	  append(o4, "),sr_energy(",12);
	  append(o4, sr_energy(llb,rrb));
	  append(o5, "),termaupenalty(",16);
	  append(o5, termau_energy(lb,rb));
	  append(o5, ")}",2);
	  string o;
	  append(o,o1);
	  append(o,o2);
	  append(o,o3);
	  append(o,o4);
	  append(o,o5);
	  res.rep = o;
    return res;
  }

  mfeanswer mldladr(Subsequence llb,Subsequence lb,Subsequence dl,mfeanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
    mfeanswer res;
    res.energy = ml_energy() + ul_energy() + e.energy + dli_energy(lb,rb) + min(dri_energy(lb,rb), dr_energy(e.rightBase,e.rightBase)) + sr_energy(llb,rrb) + termau_energy(lb,rb);
    res.leftBase = llb;
    res.rightBase = rrb;
	  string o;
	  append(o, "mldladr{", 8);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, ml_energy() + ul_energy()+dli_energy(lb,rb) + min(dri_energy(lb,rb), dr_energy(e.rightBase,e.rightBase)) + sr_energy(llb,rrb) + termau_energy(lb,rb));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer mladldr(Subsequence llb,Subsequence lb,Subsequence dl,mfeanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
    mfeanswer res;
    res.energy = ml_energy() + ul_energy() + e.energy + min(dli_energy(lb,rb), dl_energy(e.leftBase, e.leftBase)) + dri_energy(lb,rb) + sr_energy(llb,rrb) + termau_energy(lb,rb);
    res.leftBase = llb;
    res.rightBase = rrb;
	  string o;
	  append(o, "mladldr{", 8);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, ml_energy() + ul_energy()+min(dli_energy(lb,rb), dl_energy(e.leftBase, e.leftBase)) + dri_energy(lb,rb) + sr_energy(llb,rrb) + termau_energy(lb,rb));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer mldl(Subsequence llb,Subsequence lb,Subsequence dl,mfeanswer e,Subsequence rb,Subsequence rrb) {
    mfeanswer res;
    res.energy = ml_energy() + ul_energy() + e.energy + dli_energy(lb,rb) + sr_energy(llb,rrb) + termau_energy(lb,rb);
    res.leftBase = llb;
    res.rightBase = rrb;
	  string o;
	  append(o, "mldl{", 5);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, ml_energy() + ul_energy()+dli_energy(lb,rb) + sr_energy(llb,rrb) + termau_energy(lb,rb));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer mladl(Subsequence llb,Subsequence lb,Subsequence dl,mfeanswer e,Subsequence rb,Subsequence rrb) {
    mfeanswer res;
    res.energy = ml_energy() + ul_energy() + e.energy + min(dli_energy(lb,rb), dl_energy(e.leftBase, e.leftBase)) + sr_energy(llb,rrb) + termau_energy(lb,rb);
    res.leftBase = llb;
    res.rightBase = rrb;
	  string o;
	  append(o, "mladl{", 6);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, ml_energy() + ul_energy()+min(dli_energy(lb,rb), dl_energy(e.leftBase, e.leftBase)) + sr_energy(llb,rrb) + termau_energy(lb,rb));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer addss(mfeanswer e,Subsequence rb) {
    mfeanswer res;
    res.energy = e.energy + ss_energy(rb.i,rb.j);
	  Subsequence region;
	  region.i = e.leftBase.i;
	  region.j = e.rightBase.j;
	  region.seq = e.leftBase.seq;
    res.leftBase = region;
    res.rightBase = region;
	  string o;
	  append(o, "addss{", 6);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, ss_energy(rb.i,rb.j));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer ssadd(Subsequence lb,mfeanswer e) {
    mfeanswer res;
    res.energy = ul_energy() + e.energy + ss_energy(lb.i,lb.j);
	  Subsequence region;
	  region.i = e.leftBase.i;
	  region.j = e.rightBase.j;
	  region.seq = e.leftBase.seq;
    res.leftBase = region;
    res.rightBase = region;
	  string o;
	  append(o, "ssadd{40,", 9);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, ss_energy(lb.i,lb.j));
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer trafo(mfeanswer e) {
    mfeanswer res;
    res = e;
    res.leftBase = e.leftBase;
    res.rightBase = e.rightBase;
	  string o;
	  append(o, "trafo{", 6);
	  append(o, e.rep);
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer incl(mfeanswer e) {
    mfeanswer res;
    res.energy = ul_energy() + e.energy;
	  Subsequence region;
	  region.i = e.leftBase.i;
	  region.j = e.rightBase.j;
	  region.seq = e.leftBase.seq;
    res.leftBase = region;
    res.rightBase = region;
	  string o;
	  append(o, "incl{", 5);
	  append(o, e.rep);
	  append(o, ",", 1);
	  append(o, "40}", 3);
	  res.rep = o;
    return res;
  }

  mfeanswer combine(mfeanswer le,mfeanswer re) {
    mfeanswer res;
    res.energy = le.energy + re.energy;
    res.leftBase = le.leftBase;
    res.rightBase = re.rightBase;
	  string o;
	  append(o, "combine{", 8);
	  append(o, le.rep);
	  append(o, ",", 1);
	  append(o, re.rep);
	  append(o, "}", 1);
	  res.rep = o;
    return res;
  }

  mfeanswer acomb(mfeanswer le,Subsequence b,mfeanswer re) {
    mfeanswer res;
    res.energy = le.energy + re.energy + min(dr_energy(le.rightBase, le.rightBase), dl_energy(re.leftBase, re.leftBase));
    res.leftBase = le.leftBase;
    res.rightBase = re.rightBase;
	  string o1;
	  string o2;
	  string o3;
	  append(o1, "acomb{", 6);
	  append(o1, le.rep);
	  append(o2, ",", 1);
	  append(o2, re.rep);
	  append(o2, ",min(dr_energy(", 15);
	  append(o2, dr_energy(le.rightBase, le.rightBase));
	  append(o3, "),dl_energy(", 12);
	  append(o3, dl_energy(re.leftBase, re.leftBase));
	  append(o3, ")=", 2);
	  append(o3, min(dr_energy(le.rightBase, le.rightBase), dl_energy(re.leftBase, re.leftBase)));
	  append(o3, "}", 1);
	  string o;
	  append(o, o1);
	  append(o, o2);
	  append(o, o3);
	  res.rep = o;
    return res;
  }

  choice [mfeanswer] h([mfeanswer] i)
  {
	  
    return list(minimum(i));
    //~ return i;
  }
}

algebra pretty implements Canonical_Algebra(alphabet = char, answer = string)
{
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

  string cadd_Pr(string le,string re) {
    string res;
    append(res, le);
    append(res, re);
    return res;
  }

  string cadd_Pr_Pr(string le,string re) {
    string res;
    append(res, le);
    append(res, re);
    return res;
    return res;
  }

  string cadd_Pr_Pr_Pr(string le,string re) {
    string res;
    append(res, le);
    append(res, re);
    return res;
  }

  string ambd(string le,Subsequence b,string re) {
    string res;
    append(res, le);
    append(res, '.');
    append(res, re);
    return res;
  }

  string ambd_Pr(string le,Subsequence b,string re) {
    string res;
    append(res, le);
    append(res, '.');
    append(res, re);
    return res;
  }

  string nil(void) {
    string r;
    return r;
  }

  string nil_Pr(void) {
    string r;
    return r;
  }

  string edl(Subsequence lb,string e) {
    string res;
    append(res, '.');
    append(res, e);
    return res;
  }

  string edr(string e,Subsequence rb) {
    string res;
    append(res, e);
    append(res, '.');
    return res;
  }

  string edlr(Subsequence lb,string e,Subsequence rb) {
    string res;
    append(res, '.');
    append(res, e);
    append(res, '.');
    return res;
  }

  string drem(string e) {
    return e;
  }

  string is(string e) {
    return e;
  }

  string sr(Subsequence lb,string e,Subsequence rb) {
    string res;
    append(res, '(');
    append(res, e);
    append(res, ')');
    return res;
  }

  string hl(Subsequence llb,Subsequence lb,Subsequence region,Subsequence rb,Subsequence rrb) {
    string res;
    append(res, "((",2);
    append(res, '.', size(region));
    append(res, "))",2);
    return res;
  }

  string sp(Subsequence llb,Subsequence lb,string e,Subsequence rb,Subsequence rrb) {
    string res;
    append(res, "((",2);
    append(res, e);
    append(res, "))",2);
    return res;
  }

  string bl(Subsequence lregion,string e) {
    string res;
    append(res, '.', size(lregion));
    append(res, e);
    return res;
  }

  string br(string e,Subsequence rregion) {
    string res;
    append(res, e);
    append(res, '.', size(rregion));
    return res;
  }

  string il(Subsequence lregion,string e,Subsequence rregion) {
    string res;
    append(res, '.', size(lregion));
    append(res, e);
    append(res, '.', size(rregion));
    return res;
  }

  string ml(Subsequence llb,Subsequence lb,string e,Subsequence rb,Subsequence rrb) {
    string res;
    append(res, "((", 2);
    append(res, e);
    append(res, "))", 2);
    return res;
  }

  string mldr(Subsequence llb,Subsequence lb,string e,Subsequence dr,Subsequence rb,Subsequence rrb) {
    string res;
    append(res, "((", 2);
    append(res, e);
    append(res, '.');
    append(res, "))", 2);
    return res;
  }

  string mladr(Subsequence llb,Subsequence lb,string e,Subsequence dr,Subsequence rb,Subsequence rrb) {
    string res;
    append(res, "((", 2);
    append(res, e);
    append(res, '.');
    append(res, "))", 2);
    return res;
  }

  string mldlr(Subsequence llb,Subsequence lb,Subsequence dl,string e,Subsequence dr,Subsequence rb,Subsequence rrb) {
    string res;
    append(res, "((", 2);
    append(res, '.');
    append(res, e);
    append(res, '.');
    append(res, "))", 2);
    return res;
  }

  string mladlr(Subsequence llb,Subsequence lb,Subsequence dl,string e,Subsequence dr,Subsequence rb,Subsequence rrb) {
    string res;
    append(res, "((", 2);
    append(res, '.');
    append(res, e);
    append(res, '.');
    append(res, "))", 2);
    return res;
  }

  string mldladr(Subsequence llb,Subsequence lb,Subsequence dl,string e,Subsequence dr,Subsequence rb,Subsequence rrb) {
    string res;
    append(res, "((", 2);
    append(res, '.');
    append(res, e);
    append(res, '.');
    append(res, "))", 2);
    return res;
  }

  string mladldr(Subsequence llb,Subsequence lb,Subsequence dl,string e,Subsequence dr,Subsequence rb,Subsequence rrb) {
    string res;
    append(res, "((", 2);
    append(res, '.');
    append(res, e);
    append(res, '.');
    append(res, "))", 2);
    return res;
  }

  string mldl(Subsequence llb,Subsequence lb,Subsequence dl,string e,Subsequence rb,Subsequence rrb) {
    string res;
    append(res, "((", 2);
    append(res, '.');
    append(res, e);
    append(res, "))", 2);
    return res;
  }

  string mladl(Subsequence llb,Subsequence lb,Subsequence dl,string e,Subsequence rb,Subsequence rrb) {
    string res;
    append(res, "((", 2);
    append(res, '.');
    append(res, e);
    append(res, "))", 2);
    return res;
  }

  string addss(string e,Subsequence rb) {
    string res;
    append(res, e);
    append(res, '.', size(rb));
    return res;
  }

  string ssadd(Subsequence lb,string e) {
    string res;
    append(res, '.', size(lb));
    append(res, e);
    return res;
  }

  string trafo(string e) {
    return e;
  }

  string incl(string e) {
    return e;
  }

  string combine(string le,string re) {
    string res;
    append(res, le);
    append(res, re);
    return res;
  }

  string acomb(string le,Subsequence b,string re) {
    string res;
    append(res, le);
    append(res, '.');
    append(res, re);
    return res;
  }

  choice [string] h([string] i)
  {
    //~ return list(minimum(i));
    return i;
  }
}

grammar canonicals_nonamb uses Canonical_Algebra(axiom = struc) {
  struc = left_dangle | 
          trafo(noleft_dangle) | 
	  left_unpaired 
	# h;
  
  left_unpaired = sadd(BASE, left_unpaired) | 
	          sadd(BASE, left_dangle) 
		# h;

  left_dangle = ambd(edanglel, BASE, noleft_dangle) | 
                cadd_Pr(edanglel, {noleft_dangle | nil_Pr(EMPTY)}) | 
		cadd(edanglelr, {left_dangle | left_unpaired}) | 
		nil(EMPTY) 
	      # h;
  
  noleft_dangle = cadd_Pr_Pr(edangler, {left_dangle | left_unpaired}) | 
                  cadd_Pr_Pr_Pr(nodangle, {noleft_dangle | nil_Pr(EMPTY)}) | 
		  ambd_Pr(nodangle, BASE, noleft_dangle) 
		# h;

  edanglel = edl(BASE, initstem) 
	   # h;

  edangler = edr(initstem, BASE) 
	   # h;
  
  edanglelr = edlr(BASE, initstem, BASE) 
            # h;
  
  nodangle = drem(initstem) 
           # h;
  
  initstem = is(closed) 
           # h;
  
  closed = stack | 
           hairpin | 
	   multiloop | 
	   leftB | 
	   rightB | 
	   iloop 
         # h;
  
  multiloop = {mldl(BASE, BASE, BASE, ml_comps1, BASE, BASE) |
              mladl(BASE, BASE, BASE, ml_comps2, BASE, BASE) |
	      mldr(BASE, BASE, ml_comps3, BASE, BASE, BASE) |
	      mladr(BASE, BASE, ml_comps2, BASE, BASE, BASE) |
	      mldlr(BASE, BASE, BASE, ml_comps4, BASE, BASE, BASE) |
	      mladlr(BASE, BASE, BASE, ml_comps2, BASE, BASE, BASE) |
	      mldladr(BASE, BASE, BASE, ml_comps1, BASE, BASE, BASE) |
	      mladldr(BASE, BASE, BASE, ml_comps3, BASE, BASE, BASE) |
	      ml(BASE, BASE, ml_comps2, BASE, BASE)} 
	    with stackpairing;
                              
  ml_comps1 = combine(block_dl, no_dl_no_ss_end) |
              combine(block_dlr, dl_or_ss_left_no_ss_end) |
	      acomb(block_dl, BASE, no_dl_no_ss_end)
	    # h;
	    
  ml_comps2 = combine(incl(nodangle), no_dl_no_ss_end) |
              combine(incl(edangler), dl_or_ss_left_no_ss_end) |
	      acomb(incl(nodangle), BASE, no_dl_no_ss_end)
	    # h;

  ml_comps3 = combine(incl(edangler), dl_or_ss_left_ss_end) |
              combine(incl(nodangle), no_dl_ss_end) |
	      acomb(incl(nodangle), BASE, no_dl_ss_end)
	    # h;

  ml_comps4 = combine(block_dl, no_dl_ss_end) |
              combine(block_dlr, dl_or_ss_left_ss_end) |
	      acomb(block_dl, BASE, no_dl_ss_end)
	    # h;
  
  block_dl = ssadd(REGION, edanglel) |
             incl(edanglel) 
	   # h;
	   
  block_dlr = ssadd(REGION, edanglelr) |
	      incl(edanglelr) 
	    # h;
	    
  no_dl_no_ss_end = ml_comps2 |
                    incl(nodangle)
		  # h;
		  
  dl_or_ss_left_no_ss_end = ml_comps1 |
			    block_dl
			  # h;

  no_dl_ss_end = ml_comps3 |
                 incl(edangler) |
		 addss(incl(edangler), REGION)
	       # h;
	       
  dl_or_ss_left_ss_end = ml_comps4 |
                         block_dlr |
			 addss(block_dlr, REGION)
		       # h;

  stack = sr(BASE, closed, BASE)
        with basepairing;
	
  hairpin = hl(BASE, BASE, REGION with minsize(3), BASE, BASE)
          with stackpairing;
	  
  leftB = sp(BASE, BASE, bl(REGION, initstem), BASE, BASE)
        with stackpairing;
	
  rightB = sp(BASE, BASE, br(initstem, REGION), BASE, BASE)
        with stackpairing;

  iloop = sp(BASE, BASE, il(REGION with maxsize(30), closed, REGION with maxsize(30)), BASE, BASE)
        with stackpairing;
}

instance count = canonicals_nonamb ( count ) ;
instance mfe = canonicals_nonamb ( mfe ) ;
instance ppmfe = canonicals_nonamb ( pretty * mfe ) ;
instance mfepp = canonicals_nonamb ( mfe * pretty ) ;
instance pretty = canonicals_nonamb ( pretty ) ;

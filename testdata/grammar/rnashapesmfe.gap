import rna

input rna

type mfeanswer = (int energy, Subsequence firstStem, Subsequence lastStem)
type shape_t = shape


signature Canonical_Algebra(alphabet,answer) {
	answer sadd(Subsequence,answer);
	answer cadd(answer,answer);
	answer cadd_Pr(answer,answer);
	answer cadd_Pr_Pr(answer,answer);
	answer cadd_Pr_Pr_Pr(answer,answer);
	answer ambd(answer,Subsequence,answer);
	answer ambd_Pr(answer,Subsequence,answer);
	answer nil(Subsequence);
	answer nil_Pr(Subsequence);
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



algebra mfe implements Canonical_Algebra(alphabet = char, answer = mfeanswer) {
	mfeanswer sadd(Subsequence lb,mfeanswer e) {
		mfeanswer res;
		res.energy = e.energy;
		res.firstStem.seq = lb.seq;
		res.firstStem.i = lb.i;
		res.firstStem.j = e.firstStem.j;
		return res;
	}

	mfeanswer cadd(mfeanswer le,mfeanswer re) {
		mfeanswer res;
		res.energy = le.energy + re.energy;
		res.firstStem = le.firstStem;
		return res;
	}

	mfeanswer cadd_Pr(mfeanswer le,mfeanswer re) {
		mfeanswer res;
		res.energy = le.energy + re.energy;
		res.firstStem = le.firstStem;
		return res;
	}

	mfeanswer cadd_Pr_Pr(mfeanswer le,mfeanswer re) {
		mfeanswer res;
		res.energy = le.energy + re.energy;
		res.firstStem = le.firstStem;
		return res;
	}

	mfeanswer cadd_Pr_Pr_Pr(mfeanswer le,mfeanswer re) {
		mfeanswer res;
		res.energy = le.energy + re.energy;
		res.firstStem = le.firstStem;
		return res;
	}

	mfeanswer ambd(mfeanswer le,Subsequence b,mfeanswer re) {
		mfeanswer res;
		res.energy = le.energy + re.energy + min(dr_energy(le.firstStem, le.firstStem), dl_energy(re.firstStem, re.firstStem));
		res.firstStem = le.firstStem;
		return res;
	}

	mfeanswer ambd_Pr(mfeanswer le,Subsequence b,mfeanswer re) {
		mfeanswer res;
		res.energy = le.energy + re.energy + min(dr_energy(le.firstStem, le.firstStem), dl_energy(re.firstStem, re.firstStem));
		res.firstStem = le.firstStem;
		return res;
	}

	mfeanswer nil(Subsequence loc) {
		mfeanswer res;
		res.energy = 0;
		res.firstStem = loc;
		return res;
	}

	mfeanswer nil_Pr(Subsequence loc) {
		mfeanswer res;
		res.energy = 0;
		res.firstStem = loc;
		return res;
	}

	mfeanswer edl(Subsequence lb,mfeanswer e) {
		mfeanswer res;
		res.energy = e.energy + dl_energy(e.firstStem, e.firstStem);
		res.firstStem = e.firstStem;
		return res;
	}

	mfeanswer edr(mfeanswer e,Subsequence rb) {
		mfeanswer res;
		res.energy = e.energy + dr_energy(e.firstStem, e.firstStem);
		res.firstStem = e.firstStem;
		return res;
	}

	mfeanswer edlr(Subsequence lb,mfeanswer e,Subsequence rb) {
		mfeanswer res;
		res.energy = e.energy + ext_mismatch_energy(e.firstStem, e.firstStem);
		res.firstStem = e.firstStem;
		return res;
	}

	mfeanswer drem(mfeanswer e) {
		return e;
	}

	mfeanswer is(mfeanswer e) {
		mfeanswer res;
		res.energy = e.energy + termau_energy(e.firstStem, e.firstStem);
		res.firstStem = e.firstStem;
		return res;
	}

	mfeanswer sr(Subsequence lb,mfeanswer e,Subsequence rb) {
		mfeanswer res;
		res.firstStem.seq = lb.seq;
		res.firstStem.i = lb.i;
		res.firstStem.j = rb.j;
		
		res.energy = e.energy + sr_energy(res.firstStem,res.firstStem);
		return res;
	}

	mfeanswer hl(Subsequence llb,Subsequence lb,Subsequence region,Subsequence rb,Subsequence rrb) {
		mfeanswer res;
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = hl_energy(region) + sr_energy(res.firstStem,res.firstStem);
		return res;
	}

	mfeanswer sp(Subsequence llb,Subsequence lb,mfeanswer e,Subsequence rb,Subsequence rrb) {
		mfeanswer res;
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		res.energy = e.energy + sr_energy(res.firstStem,res.firstStem);
		return res;
	}

	mfeanswer bl(Subsequence lregion,mfeanswer e) {
		mfeanswer res;
		res.firstStem.seq = lregion.seq;
		res.firstStem.i = lregion.i;
		res.firstStem.j = e.firstStem.j;
		
		Subsequence innerStem;
		innerStem.seq = lregion.seq;
		innerStem.i = lregion.i-1;
		innerStem.j = e.firstStem.j+1;
		
		res.energy = e.energy + bl_energy(lregion, innerStem);
		return res;
	}

	mfeanswer br(mfeanswer e,Subsequence rregion) {
		mfeanswer res;
		res.firstStem.seq = rregion.seq;
		res.firstStem.i = e.firstStem.i;
		res.firstStem.j = rregion.j;
		
		Subsequence innerStem;
		innerStem.seq = rregion.seq;
		innerStem.i = e.firstStem.i-1;
		innerStem.j = rregion.j+1;
		
		res.energy = e.energy + br_energy(innerStem, rregion);  
		return res;
	}

	mfeanswer il(Subsequence lregion,mfeanswer e,Subsequence rregion) {
		mfeanswer res;
		res.firstStem.seq = lregion.seq;
		res.firstStem.i = lregion.i;
		res.firstStem.j = rregion.j;
		
		res.energy = e.energy + il_energy(lregion, rregion);
		return res;
	}

	mfeanswer ml(Subsequence llb,Subsequence lb,mfeanswer e,Subsequence rb,Subsequence rrb) {
		mfeanswer res;
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = ml_energy() + ul_energy() + e.energy + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem);
		return res;
	}

	mfeanswer mldr(Subsequence llb,Subsequence lb,mfeanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		mfeanswer res;
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = ml_energy() + ul_energy() + e.energy + dri_energy(innerStem,innerStem) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem);
		return res;
	}

	mfeanswer mladr(Subsequence llb,Subsequence lb,mfeanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		mfeanswer res;
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = ml_energy() + ul_energy() + e.energy + min(dri_energy(innerStem,innerStem), dr_energy(e.lastStem, e.lastStem)) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem);
		return res;
	}

	mfeanswer mldlr(Subsequence llb,Subsequence lb,Subsequence dl,mfeanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		mfeanswer res;
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = ml_energy() + ul_energy() + e.energy + ml_mismatch_energy(innerStem,innerStem) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem);
		return res;
	}

	mfeanswer mladlr(Subsequence llb,Subsequence lb,Subsequence dl,mfeanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		mfeanswer res;
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = ml_energy() + ul_energy() + e.energy + min(dli_energy(innerStem,innerStem), dl_energy(e.firstStem, e.firstStem)) + min(dri_energy(innerStem,innerStem), dr_energy(e.lastStem, e.lastStem)) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem);
		return res;
	}

	mfeanswer mldladr(Subsequence llb,Subsequence lb,Subsequence dl,mfeanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		mfeanswer res;
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = ml_energy() + ul_energy() + e.energy + dli_energy(innerStem,innerStem) + min(dri_energy(innerStem,innerStem), dr_energy(e.lastStem,e.lastStem)) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem);
		return res;
	}

	mfeanswer mladldr(Subsequence llb,Subsequence lb,Subsequence dl,mfeanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		mfeanswer res;
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = ml_energy() + ul_energy() + e.energy + min(dli_energy(innerStem,innerStem), dl_energy(e.firstStem, e.firstStem)) + dri_energy(innerStem,innerStem) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem);
		return res;
	}

	mfeanswer mldl(Subsequence llb,Subsequence lb,Subsequence dl,mfeanswer e,Subsequence rb,Subsequence rrb) {
		mfeanswer res;
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = ml_energy() + ul_energy() + e.energy + dli_energy(innerStem,innerStem) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem);
		return res;
	}

	mfeanswer mladl(Subsequence llb,Subsequence lb,Subsequence dl,mfeanswer e,Subsequence rb,Subsequence rrb) {
		mfeanswer res;
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = ml_energy() + ul_energy() + e.energy + min(dli_energy(innerStem,innerStem), dl_energy(e.firstStem, e.firstStem)) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem);
		return res;
	}

	mfeanswer addss(mfeanswer e,Subsequence rb) {
		mfeanswer res;
		res.energy = e.energy + ss_energy(rb);
		
		res.firstStem = e.firstStem;
		res.lastStem = e.lastStem;
		return res;
	}

	mfeanswer ssadd(Subsequence lb,mfeanswer e) {
		mfeanswer res;
		res.energy = ul_energy() + e.energy + ss_energy(lb);
		
		res.firstStem = e.firstStem;
		res.lastStem = e.firstStem;
		return res;
	}

	mfeanswer trafo(mfeanswer e) {
		return e;
	}

	mfeanswer incl(mfeanswer e) {
		mfeanswer res;
		res.energy = ul_energy() + e.energy;
		
		res.firstStem = e.firstStem;
		res.lastStem = e.firstStem;
		return res;
	}

	mfeanswer combine(mfeanswer le,mfeanswer re) {
		mfeanswer res;
		res.energy = le.energy + re.energy;
		
		res.firstStem = le.firstStem;
		res.lastStem = re.lastStem;
		return res;
	}

	mfeanswer acomb(mfeanswer le,Subsequence b,mfeanswer re) {
		mfeanswer res;
		res.energy = le.energy + re.energy + min(dr_energy(le.lastStem, le.lastStem), dl_energy(re.firstStem, re.firstStem));
		res.firstStem = le.firstStem;
		res.lastStem = re.lastStem;
		return res;
	}

	choice [mfeanswer] h([mfeanswer] i) {
		return list(minimum(i));
	}
}

algebra pretty implements Canonical_Algebra(alphabet = char, answer = string) {
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

	string nil(Subsequence loc) {
		string r;
		return r;
	}

	string nil_Pr(Subsequence loc) {
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

	choice [string] h([string] i) {
		//~ return list(minimum(i));
		return i;
	}
}


algebra shape5 implements Canonical_Algebra(alphabet = char, answer = shape_t) {
	shape_t sadd(Subsequence b,shape_t e) {
		return e;
	}

	shape_t cadd(shape_t le,shape_t re) {
		shape_t res;
		append(res, le);
		append(res, re);
		return res;
	}

	shape_t cadd_Pr(shape_t le,shape_t re) {
		shape_t res;
		append(res, le);
		append(res, re);
		return res;
	}

	shape_t cadd_Pr_Pr(shape_t le,shape_t re) {
		shape_t res;
		append(res, le);
		append(res, re);
		return res;
	}

	shape_t cadd_Pr_Pr_Pr(shape_t le,shape_t re) {
		shape_t res;
		append(res, le);
		append(res, re);
		return res;
	}

	shape_t ambd(shape_t le,Subsequence b,shape_t re) {
		shape_t res;
		append(res, le);
		append(res, re);
		return res;
	}

	shape_t ambd_Pr(shape_t le,Subsequence b,shape_t re) {
		shape_t res;
		append(res, le);
		append(res, re);
		return res;
	}

	shape_t nil(Subsequence loc) {
		shape_t r;
		return r;
	}

	shape_t nil_Pr(Subsequence loc) {
		shape_t r;
		return r;
	}

	shape_t edl(Subsequence lb,shape_t e) {
		shape_t res;
		append(res, '[');
		append(res, e);
		append(res, ']');
		return res;
	}

	shape_t edr(shape_t e,Subsequence rb) {
		shape_t res;
		append(res, '[');
		append(res, e);
		append(res, ']');
		return res;
	}

	shape_t edlr(Subsequence lb,shape_t e,Subsequence rb) {
		shape_t res;
		append(res, '[');
		append(res, e);
		append(res, ']');
		return res;
	}

	shape_t drem(shape_t e) {
		shape_t res;
		append(res, '[');
		append(res, e);
		append(res, ']');
		return res;
	}

	shape_t is(shape_t e) {
		return e;
	}

	shape_t sr(Subsequence lb,shape_t e,Subsequence rb) {
		return e;
	}

	shape_t hl(Subsequence llb,Subsequence lb,Subsequence region,Subsequence rb,Subsequence rrb) {
		shape_t res;
		return res;
	}

	shape_t sp(Subsequence llb,Subsequence lb,shape_t e,Subsequence rb,Subsequence rrb) {
		return e;
	}

	shape_t bl(Subsequence lregion,shape_t e) {
		return e;
	}

	shape_t br(shape_t e,Subsequence rregion) {
		return e;
	}

	shape_t il(Subsequence lregion,shape_t e,Subsequence rregion) {
		return e;
	}

	shape_t ml(Subsequence llb,Subsequence lb,shape_t e,Subsequence rb,Subsequence rrb) {
		return e;
	}

	shape_t mldr(Subsequence llb,Subsequence lb,shape_t e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		return e;
	}

	shape_t mladr(Subsequence llb,Subsequence lb,shape_t e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		return e;
	}

	shape_t mldlr(Subsequence llb,Subsequence lb,Subsequence dl,shape_t e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		return e;
	}

	shape_t mladlr(Subsequence llb,Subsequence lb,Subsequence dl,shape_t e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		return e;
	}

	shape_t mldladr(Subsequence llb,Subsequence lb,Subsequence dl,shape_t e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		return e;
	}

	shape_t mladldr(Subsequence llb,Subsequence lb,Subsequence dl,shape_t e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		return e;
	}

	shape_t mldl(Subsequence llb,Subsequence lb,Subsequence dl,shape_t e,Subsequence rb,Subsequence rrb) {
		return e;
	}

	shape_t mladl(Subsequence llb,Subsequence lb,Subsequence dl,shape_t e,Subsequence rb,Subsequence rrb) {
		return e;
	}

	shape_t addss(shape_t e,Subsequence rb) {
		return e;
	}

	shape_t ssadd(Subsequence lb,shape_t e) {
		return e;
	}

	shape_t trafo(shape_t e) {
		return e;
	}

	shape_t incl(shape_t e) {
		return e;
	}

	shape_t combine(shape_t le,shape_t re) {
		shape_t res;
		append(res, le);
		append(res, re);
		return res;
	}

	shape_t acomb(shape_t le,Subsequence b,shape_t re) {
		shape_t res;
		append(res, le);
		append(res, re);
		return res;
	}

	choice [shape_t] h([shape_t] i) {
		return unique(i);
	}
}

grammar canonicals_nonamb uses Canonical_Algebra(axiom = struc) {
	struc = left_dangle | trafo(noleft_dangle) | left_unpaired # h;

	left_unpaired = sadd(BASE, left_unpaired) | sadd(BASE, left_dangle) # h;

	left_dangle = ambd(edanglel, BASE, noleft_dangle) | cadd_Pr(edanglel, {noleft_dangle | nil_Pr(LOC)}) | cadd(edanglelr, {left_dangle | left_unpaired}) | nil(LOC) # h;

	noleft_dangle = cadd_Pr_Pr(edangler, {left_dangle | left_unpaired}) | cadd_Pr_Pr_Pr(nodangle, {noleft_dangle | nil_Pr(LOC)}) | ambd_Pr(nodangle, BASE, noleft_dangle) # h;

	edanglel = edl(BASE, initstem) # h;

	edangler = edr(initstem, BASE) # h;

	edanglelr = edlr(BASE, initstem, BASE) # h;

	nodangle = drem(initstem) # h;

	initstem = is(closed) # h;

	closed = stack | hairpin | multiloop | leftB | rightB | iloop # h;

	multiloop = {mldl(BASE, BASE, BASE, ml_comps1, BASE, BASE) | 
		mladl(BASE, BASE, BASE, ml_comps2, BASE, BASE) | 
		mldr(BASE, BASE, ml_comps3, BASE, BASE, BASE) | 
		mladr(BASE, BASE, ml_comps2, BASE, BASE, BASE) | 
		mldlr(BASE, BASE, BASE, ml_comps4, BASE, BASE, BASE) | 
		mladlr(BASE, BASE, BASE, ml_comps2, BASE, BASE, BASE) | 
		mldladr(BASE, BASE, BASE, ml_comps1, BASE, BASE, BASE) | 
		mladldr(BASE, BASE, BASE, ml_comps3, BASE, BASE, BASE)  | 
		ml(BASE, BASE, ml_comps2, BASE, BASE)} with stackpairing;

	ml_comps1 = combine(block_dl, no_dl_no_ss_end) | combine(block_dlr, dl_or_ss_left_no_ss_end) | acomb(block_dl, BASE, no_dl_no_ss_end) # h;

	ml_comps2 = combine(incl(nodangle), no_dl_no_ss_end) | combine(incl(edangler), dl_or_ss_left_no_ss_end) | acomb(incl(nodangle), BASE, no_dl_no_ss_end) # h;

	ml_comps3 = combine(incl(edangler), dl_or_ss_left_ss_end) | combine(incl(nodangle), no_dl_ss_end) | acomb(incl(nodangle), BASE, no_dl_ss_end) # h;

	ml_comps4 = combine(block_dl, no_dl_ss_end) | combine(block_dlr, dl_or_ss_left_ss_end) | acomb(block_dl, BASE, no_dl_ss_end) # h;

	block_dl = ssadd(REGION, edanglel) | incl(edanglel) # h;

	block_dlr = ssadd(REGION, edanglelr) | incl(edanglelr) # h;

	no_dl_no_ss_end = ml_comps2 | incl(nodangle) # h;

	dl_or_ss_left_no_ss_end = ml_comps1 | block_dl # h;

	no_dl_ss_end = ml_comps3 | incl(edangler) | addss(incl(edangler), REGION) # h;

	dl_or_ss_left_ss_end = ml_comps4 | block_dlr | addss(block_dlr, REGION) # h;

	stack = sr(BASE, closed, BASE) with basepairing # h;

	hairpin = hl(BASE, BASE, REGION with minsize(3), BASE, BASE) with stackpairing # h;

	leftB = sp(BASE, BASE, bl(REGION, closed), BASE, BASE) with stackpairing # h;

	rightB = sp(BASE, BASE, br(closed, REGION), BASE, BASE) with stackpairing # h;

	iloop = sp(BASE, BASE, il(REGION with maxsize(30), closed, REGION with maxsize(30)), BASE, BASE) with stackpairing # h;

}

instance count = canonicals_nonamb ( count ) ;
instance mfe = canonicals_nonamb ( mfe ) ;
instance ppmfe = canonicals_nonamb ( pretty * mfe ) ;
instance ppenmfe = canonicals_nonamb ( (pretty * enum) * mfe ) ;
instance mfepp = canonicals_nonamb ( mfe * pretty ) ;
instance mfeppshape = canonicals_nonamb ( mfe * (pretty * shape5) ) ;
instance mfeppen = canonicals_nonamb ( mfe * (pretty * enum) ) ;
instance pretty = canonicals_nonamb ( pretty ) ;
instance shape5 = canonicals_nonamb ( (shape5 * mfe) * pretty) ;

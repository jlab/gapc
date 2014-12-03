import rna
import nonamb_answer

input rna


type pftuple = extern
type pfanswer = extern
type mfeanswer = (int energy, Subsequence firstStem, Subsequence lastStem, string rep)
type shape_t = shape
type base_t = extern

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

algebra enumi auto enum ;

algebra p_func implements Canonical_Algebra(alphabet = char, answer = pfanswer) {
	pfanswer sadd(Subsequence lb,pfanswer e) {
		pfanswer res = e;
		
		res.pf.q1 = scale(1) * e.pf.q1 * mk_pf(sbase_energy());
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword.i = lb.i;
		
		return res;
	}

	pfanswer cadd(pfanswer le,pfanswer re) {
		pfanswer res = le;
		
		res.pf.q1 = le.pf.q1 * re.pf.q1;
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		res.subword.i = le.subword.i;
		res.subword.j = re.subword.j;
		
		return res;
	}

	pfanswer cadd_Pr(pfanswer le,pfanswer re) {
		pfanswer res = le;
		
		res.pf.q1 = le.pf.q1 * sum_elems(re.pf);
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		res.subword.i = le.subword.i;
		res.subword.j = re.subword.j;
		
		return res;
	}

	pfanswer cadd_Pr_Pr(pfanswer le,pfanswer re) {
		pfanswer res = le;
		
		res.pf = mk_tuple(le.firststem, le.pf.q1 * re.pf.q1);
		res.subword.i = le.subword.i;
		res.subword.j = re.subword.j;
		
		return res;
	}

	pfanswer cadd_Pr_Pr_Pr(pfanswer le,pfanswer re) {
		pfanswer res = le;
		
		res.pf = mk_tuple(le.firststem, le.pf.q1 * sum_elems(re.pf));
		
		res.subword.i = le.subword.i;
		res.subword.j = re.subword.j;
		
		return res;
	}

	pfanswer ambd(pfanswer le,Subsequence b,pfanswer re) {
		pfanswer res = le;
		
		res.pf.q1 = scale(1) * check_tuple(le.pf.q1, le.firststem, re.firststem, b, re.pf);
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		res.subword.i = le.subword.i;
		res.subword.j = re.subword.j;
		
		return res;
	}

	pfanswer ambd_Pr(pfanswer le,Subsequence b,pfanswer re) {
		pfanswer res = le;
		
		res.pf = mk_tuple(le.firststem, scale(1) * check_tuple(le.pf.q1, le.firststem, re.firststem, b, re.pf));
		res.subword.i = le.subword.i;
		res.subword.j = re.subword.j;
		
		return res;
	}

	pfanswer nil(Subsequence loc) {
		pfanswer res;
		
		res.pf.q1 = 1.0;
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		res.subword.i = 0;
		res.subword.j = seq_size(loc);
		res.subword.seq = loc.seq;
		res.firststem.i = seq_size(loc);
		res.firststem.j = seq_size(loc);
		res.firststem.seq = loc.seq;
		
		return res;
	}

	pfanswer nil_Pr(Subsequence loc) {
		pfanswer res;
		
		res.pf.q1 = 1.0;
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		res.subword.i = 0;
		res.subword.j = seq_size(loc);
		res.subword.seq = loc.seq;
		res.firststem.i = seq_size(loc);
		res.firststem.j = seq_size(loc);
		res.firststem.seq = loc.seq;
		
		return res;
	}

	pfanswer edl(Subsequence lb,pfanswer e) {
		pfanswer res = e;
		
		res.pf.q1 = scale(1) * e.pf.q1 * mk_pf(dl_energy(e.firststem, e.firststem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		res.subword.i = lb.i;
		
		return res;
	}

	pfanswer edr(pfanswer e,Subsequence rb) {
		pfanswer res = e;
		
		res.pf.q1 = scale(1) * e.pf.q1 * mk_pf(dr_energy(e.firststem, e.firststem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		res.subword.j = rb.j;
		
		return res;
	}

	pfanswer edlr(Subsequence lb,pfanswer e,Subsequence rb) {
		pfanswer res = e;
		
		res.pf.q1 = scale(2) * e.pf.q1 * mk_pf(ext_mismatch_energy(e.firststem, e.firststem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		res.subword.i = lb.i;
		res.subword.j = rb.j;
		
		return res;
	}

	pfanswer drem(pfanswer e) {
		return e;
	}

	pfanswer is(pfanswer e) {
		pfanswer res = e;
		
		res.pf.q1 = e.pf.q1 * mk_pf(termau_energy(e.firststem, e.firststem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		return res;
	}

	pfanswer sr(Subsequence lb,pfanswer e,Subsequence rb) {
		pfanswer res = e;
		
		res.subword.i = lb.i;
		res.subword.j = rb.j;
		res.firststem.i = lb.i;
		res.firststem.j = rb.j;
		
		res.pf.q1 = scale(2) * e.pf.q1 * mk_pf(sr_energy(res.firststem,res.firststem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		return res;
	}

	pfanswer hl(Subsequence llb,Subsequence lb,Subsequence region,Subsequence rb,Subsequence rrb) {
		pfanswer res;
		
		res.firststem.seq = llb.seq;
		res.firststem.i = llb.i;
		res.firststem.j = rrb.j;
		
		Subsequence innerstem;
		innerstem.seq = lb.seq;
		innerstem.i = lb.i;
		innerstem.j = rb.j;
		
		res.pf.q1 = scale(region.j - region.i + 4) * mk_pf(hl_energy(region) + sr_energy(res.firststem,res.firststem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword = res.firststem;
		
		return res;
	}

	pfanswer sp(Subsequence llb,Subsequence lb,pfanswer e,Subsequence rb,Subsequence rrb) {
		pfanswer res = e;
		
		res.firststem.i = llb.i;
		res.firststem.j = rrb.j;
		
		res.pf.q1 = scale(4) * e.pf.q1 * mk_pf(sr_energy(res.firststem,res.firststem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword = res.firststem;
		
		return res;
	}

	pfanswer bl(Subsequence lregion,pfanswer e) {
		pfanswer res = e;
		
		res.firststem.i = lregion.i;
		
		Subsequence innerstem;
		innerstem.seq = lregion.seq;
		innerstem.i = lregion.i-1;
		innerstem.j = e.firststem.j+1;
		
		Subsequence rb;
		rb.seq = lregion.seq;
		rb.i = e.firststem.j;
		rb.j = e.firststem.j+1;
		
		res.pf.q1 = scale(lregion.j - lregion.i) * e.pf.q1 * mk_pf(bl_energy(lregion,rb));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword.i = lregion.i;
		
		return res;
	}

	pfanswer br(pfanswer e,Subsequence rregion) {
		pfanswer res = e;
		
		res.firststem.j = rregion.j;
		
		Subsequence innerstem;
		innerstem.seq = rregion.seq;
		innerstem.i = e.firststem.i-1;
		innerstem.j = rregion.j+1;
		
		Subsequence lb;
		lb.seq = rregion.seq;
		lb.i = e.firststem.i-1;
		lb.j = e.firststem.i;

		
		res.pf.q1 = scale(rregion.j - rregion.i) * e.pf.q1 * mk_pf(br_energy(lb, rregion));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;

		res.subword.j = rregion.j;
		
		return res;
	}

	pfanswer il(Subsequence lregion,pfanswer e,Subsequence rregion) {
		pfanswer res = e;
		
		res.firststem.i = lregion.i;
		res.firststem.j = rregion.j;
		
		res.pf.q1 = scale((lregion.j - lregion.i) + (rregion.j - rregion.i)) * e.pf.q1 * mk_pf(il_energy(lregion, rregion));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword.i = lregion.i;
		res.subword.j = rregion.j;
		
		return res;
	}

	pfanswer ml(Subsequence llb,Subsequence lb,pfanswer e,Subsequence rb,Subsequence rrb) {
		pfanswer res = e;
		
		res.firststem.i = llb.i;
		res.firststem.j = rrb.j;
		
		Subsequence innerstem;
		innerstem.seq = lb.seq;
		innerstem.i = lb.i;
		innerstem.j = rb.j;
		
		res.pf.q1 = scale(4) * sum_elems(e.pf) * mk_pf(ml_energy() + ul_energy() + sr_energy(res.firststem,res.firststem) + termau_energy(innerstem,innerstem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword = res.firststem;
		
		return res;
	}
	
	pfanswer mldr(Subsequence llb,Subsequence lb,pfanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		pfanswer res = e;
		
		res.firststem.i = llb.i;
		res.firststem.j = rrb.j;
		
		Subsequence innerstem;
		innerstem.seq = lb.seq;
		innerstem.i = lb.i;
		innerstem.j = rb.j;
		
		res.pf.q1 = scale(5) * sum_elems(e.pf) * mk_pf(ml_energy() + ul_energy() + dri_energy(innerstem,innerstem) + sr_energy(res.firststem,res.firststem) + termau_energy(innerstem,innerstem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword = res.firststem;
		
		return res;
	}

	pfanswer mladr(Subsequence llb,Subsequence lb,pfanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		pfanswer res = e;
		
		res.firststem.i = llb.i;
		res.firststem.j = rrb.j;
		
		Subsequence innerstem;
		innerstem.seq = lb.seq;
		innerstem.i = lb.i;
		innerstem.j = rb.j;
		
		base_t rightdanglingBase = base_t(dr[dr.i]);
		base_t rightmostBaselastStem = base_t(e.subword[dr.i-1]);
		float amdangle;
		amdangle = (e.pf.q1 + e.pf.q3) * mk_pf(min(dr_dangle_dg( wc_comp(rightmostBaselastStem), rightmostBaselastStem, rightdanglingBase), dri_energy(innerstem,innerstem))) +
			   (e.pf.q2 + e.pf.q4) * mk_pf(min(dr_dangle_dg(wob_comp(rightmostBaselastStem), rightmostBaselastStem, rightdanglingBase), dri_energy(innerstem,innerstem)));
		
		res.pf.q1 = scale(5) * amdangle * mk_pf(ml_energy() + ul_energy() + sr_energy(res.firststem,res.firststem) + termau_energy(innerstem,innerstem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword = res.firststem;
		
		return res;
	}

	pfanswer mldlr(Subsequence llb,Subsequence lb,Subsequence dl,pfanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		pfanswer res = e;
		
		res.firststem.i = llb.i;
		res.firststem.j = rrb.j;
		
		Subsequence innerstem;
		innerstem.seq = lb.seq;
		innerstem.i = lb.i;
		innerstem.j = rb.j;
		
		res.pf.q1 = scale(6) * sum_elems(e.pf) * mk_pf(ml_energy() + ul_energy() + ml_mismatch_energy(innerstem,innerstem) + sr_energy(res.firststem,res.firststem) + termau_energy(innerstem,innerstem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword = res.firststem;
		
		return res;
	}

	pfanswer mladlr(Subsequence llb,Subsequence lb,Subsequence dl,pfanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		pfanswer res = e;
		
		res.firststem.i = llb.i;
		res.firststem.j = rrb.j;
		
		Subsequence innerstem;
		innerstem.seq = lb.seq;
		innerstem.i = lb.i;
		innerstem.j = rb.j;
		
		base_t leftdanglingBase = base_t(dl[dl.i]);
		base_t rightdanglingBase = base_t(dr[dr.i]);
		base_t leftmostBasefirstStem = base_t(e.subword[dl.i+1]);
		base_t rightmostBaselastStem = base_t(e.subword[dr.i-1]);
		float amdangle;
		amdangle = e.pf.q1 * mk_pf(min(dl_dangle_dg(leftdanglingBase, leftmostBasefirstStem,  wc_comp(leftmostBasefirstStem)), dli_energy(innerstem,innerstem)) + min(dr_dangle_dg( wc_comp(rightmostBaselastStem), rightmostBaselastStem, rightdanglingBase), dri_energy(innerstem,innerstem))) +
			   e.pf.q2 * mk_pf(min(dl_dangle_dg(leftdanglingBase, leftmostBasefirstStem,  wc_comp(leftmostBasefirstStem)), dli_energy(innerstem,innerstem)) + min(dr_dangle_dg(wob_comp(rightmostBaselastStem), rightmostBaselastStem, rightdanglingBase), dri_energy(innerstem,innerstem))) +
			   e.pf.q3 * mk_pf(min(dl_dangle_dg(leftdanglingBase, leftmostBasefirstStem, wob_comp(leftmostBasefirstStem)), dli_energy(innerstem,innerstem)) + min(dr_dangle_dg( wc_comp(rightmostBaselastStem), rightmostBaselastStem, rightdanglingBase), dri_energy(innerstem,innerstem))) +
			   e.pf.q4 * mk_pf(min(dl_dangle_dg(leftdanglingBase, leftmostBasefirstStem, wob_comp(leftmostBasefirstStem)), dli_energy(innerstem,innerstem)) + min(dr_dangle_dg(wob_comp(rightmostBaselastStem), rightmostBaselastStem, rightdanglingBase), dri_energy(innerstem,innerstem)));
		
		res.pf.q1 = scale(6) * amdangle * mk_pf(ml_energy() + ul_energy() + sr_energy(res.firststem, res.firststem) + termau_energy(innerstem,innerstem));
		//~ res.pf.q1 = scale(6) * sum_elems(e.pf) * mk_pf(ml_energy() + ul_energy() + sr_energy(res.firststem,res.firststem) + termau_energy(innerstem,innerstem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword = res.firststem;
		
		return res;
	}

	pfanswer mldladr(Subsequence llb,Subsequence lb,Subsequence dl,pfanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		pfanswer res = e;
		
		res.firststem.i = llb.i;
		res.firststem.j = rrb.j;
		
		Subsequence innerstem;
		innerstem.seq = lb.seq;
		innerstem.i = lb.i;
		innerstem.j = rb.j;
		
		base_t rightdanglingBase = base_t(dr[dr.i]);
		base_t rightmostBaselastStem = base_t(e.subword[dr.i-1]);
		double amdangle;
		amdangle = (e.pf.q1 * mk_pf(dli_energy(innerstem,innerstem)) + e.pf.q3) * mk_pf(min(dr_dangle_dg(wc_comp(rightmostBaselastStem), rightmostBaselastStem, rightdanglingBase), dri_energy(innerstem,innerstem))) +
			   (e.pf.q2 + e.pf.q4) * mk_pf(min(dr_dangle_dg(wob_comp(rightmostBaselastStem), rightmostBaselastStem, rightdanglingBase), dri_energy(innerstem,innerstem)));
		
		res.pf.q1 = scale(6) * amdangle * mk_pf(ml_energy() + ul_energy() + sr_energy(res.firststem,res.firststem) + termau_energy(innerstem,innerstem));
		//~ res.pf.q1 = scale(6) * sum_elems(e.pf) * mk_pf(ml_energy() + ul_energy() + sr_energy(res.firststem,res.firststem) + termau_energy(innerstem,innerstem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword = res.firststem;
		
		return res;
	}

	pfanswer mladldr(Subsequence llb,Subsequence lb,Subsequence dl,pfanswer e,Subsequence dr,Subsequence rb,Subsequence rrb) {
		pfanswer res = e;
		
		res.firststem.i = llb.i;
		res.firststem.j = rrb.j;
		
		Subsequence innerstem;
		innerstem.seq = lb.seq;
		innerstem.i = lb.i;
		innerstem.j = rb.j;
		
		base_t leftdanglingBase = base_t(dl[dl.i]);
		base_t leftmostBasefirstStem = base_t(e.subword[dl.i+1]);
		float amdangle;
		amdangle = (e.pf.q1 + e.pf.q2) * mk_pf(min(dl_dangle_dg(leftdanglingBase, leftmostBasefirstStem, wc_comp(leftmostBasefirstStem)), dli_energy(innerstem,innerstem))) +
			   (e.pf.q3 + e.pf.q4 * mk_pf(dri_energy(innerstem,innerstem))) * mk_pf(min(dl_dangle_dg(leftdanglingBase, leftmostBasefirstStem, wob_comp(leftmostBasefirstStem)), dli_energy(innerstem,innerstem)));
		
		res.pf.q1 = scale(6) * amdangle * mk_pf(ml_energy() + ul_energy() + sr_energy(res.firststem,res.firststem) + termau_energy(innerstem,innerstem));
		//~ res.pf.q1 = scale(6) * sum_elems(e.pf) * mk_pf(ml_energy() + ul_energy() + sr_energy(res.firststem,res.firststem) + termau_energy(innerstem,innerstem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword = res.firststem;
		
		return res;
	}

	pfanswer mldl(Subsequence llb,Subsequence lb,Subsequence dl,pfanswer e,Subsequence rb,Subsequence rrb) {
		pfanswer res = e;
		
		res.firststem.i = llb.i;
		res.firststem.j = rrb.j;
		
		Subsequence innerstem;
		innerstem.seq = lb.seq;
		innerstem.i = lb.i;
		innerstem.j = rb.j;
		
		res.pf.q1 = scale(5) * sum_elems(e.pf) * mk_pf(ml_energy() + ul_energy() + dli_energy(innerstem,innerstem) + sr_energy(res.firststem,res.firststem) + termau_energy(innerstem,innerstem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword = res.firststem;
		
		return res;
	}

	pfanswer mladl(Subsequence llb,Subsequence lb,Subsequence dl,pfanswer e,Subsequence rb,Subsequence rrb) {
		pfanswer res = e;
		
		res.firststem.i = llb.i;
		res.firststem.j = rrb.j;
		
		Subsequence innerstem;
		innerstem.seq = lb.seq;
		innerstem.i = lb.i;
		innerstem.j = rb.j;
		
		base_t leftdanglingBase = base_t(dl[dl.i]);
		base_t leftmostBasefirstStem = base_t(e.subword[dl.i+1]);
		float amdangle;
		amdangle = (e.pf.q1 + e.pf.q2) * mk_pf(min(dl_dangle_dg(leftdanglingBase, leftmostBasefirstStem,  wc_comp(leftmostBasefirstStem)), dli_energy(innerstem,innerstem))) +
			   (e.pf.q3 + e.pf.q4) * mk_pf(min(dl_dangle_dg(leftdanglingBase, leftmostBasefirstStem, wob_comp(leftmostBasefirstStem)), dli_energy(innerstem,innerstem)));
		
		res.pf.q1 = scale(5) * amdangle * mk_pf(ml_energy() + ul_energy() + sr_energy(res.firststem,res.firststem) + termau_energy(innerstem,innerstem));
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		res.subword = res.firststem;
		
		return res;
	}

	pfanswer addss(pfanswer e,Subsequence rregion) {
		pfanswer res = e;
		
		res.pf = mult_tup(scale(rregion.j - rregion.i) * mk_pf(ss_energy(rregion)), e.pf);

		res.subword.j = rregion.j;
		
		return res;
	}

	pfanswer ssadd(Subsequence lregion,pfanswer e) {
		pfanswer res = e;
		
		Subsequence test;
		test.seq = lregion.seq;
		test.i = lregion.i;
		test.j = lregion.j+1;

		res.pf = mk_tuple(e.firststem, scale(lregion.j - lregion.i) * e.pf.q1 * mk_pf(ul_energy() + ss_energy(lregion)));
		
		res.subword.i = lregion.i;
		
		return res;
	}

	pfanswer trafo(pfanswer e) {
		pfanswer res = e;
		
		res.pf.q1 = sum_elems(e.pf);
		res.pf.q2 = 0.0;
		res.pf.q3 = 0.0;
		res.pf.q4 = 0.0;
		
		return res;
	}

	pfanswer incl(pfanswer e) {
		pfanswer res = e;
		
		res.pf = mk_tuple(e.firststem, e.pf.q1 * mk_pf(ul_energy()));

		return res;
	}

	pfanswer combine(pfanswer le,pfanswer re) {
		pfanswer res = le;
		
		res.firststem = le.firststem;
		
		//~ res.pf = comb_tup(le.pf, re.pf);
		res.pf.q1 = (le.pf.q1 + le.pf.q2) * (re.pf.q1 + re.pf.q3);
		res.pf.q2 = (le.pf.q1 + le.pf.q2) * (re.pf.q2 + re.pf.q4);
		res.pf.q3 = (le.pf.q3 + le.pf.q4) * (re.pf.q3 + re.pf.q1);
		res.pf.q4 = (le.pf.q4 + le.pf.q3) * (re.pf.q4 + re.pf.q2);
		
		res.subword.i = le.subword.i;
		res.subword.j = re.subword.j;
		
		return res;
	}

	pfanswer acomb(pfanswer le,Subsequence b,pfanswer re) {
		pfanswer res = le;
		
		res.firststem = le.firststem;
		
		//~ res.pf = mult_tup(scale(1), acomb_tup(le.pf, n, re.pf));
		base_t baseLeftStem = base_t(le.subword[b.i-1]);
		base_t baseRightStem = base_t(re.subword[b.i+1]);
		base_t baseAmbigious = base_t(b[b.i]);
		double  wcDr = dr_dangle_dg(  wc_comp(baseLeftStem), baseLeftStem, baseAmbigious);
		double wobDr = dr_dangle_dg( wob_comp(baseLeftStem), baseLeftStem, baseAmbigious);
		double  wcDl = dl_dangle_dg(baseAmbigious, baseRightStem,  wc_comp(baseRightStem));
		double wobDl = dl_dangle_dg(baseAmbigious, baseRightStem, wob_comp(baseRightStem));
		
		res.pf.q1 = le.pf.q1 * (re.pf.q1 * mk_pf(min( wcDr, wcDl)) + re.pf.q3 * mk_pf(min( wcDr,wobDl))) + 
			    le.pf.q2 * (re.pf.q1 * mk_pf(min(wobDr, wcDl)) + re.pf.q3 * mk_pf(min(wobDr,wobDl)));
		res.pf.q2 = le.pf.q2 * (re.pf.q2 * mk_pf(min(wobDr, wcDl)) + re.pf.q4 * mk_pf(min(wobDr,wobDl))) + 
			    le.pf.q1 * (re.pf.q2 * mk_pf(min( wcDr, wcDl)) + re.pf.q4 * mk_pf(min( wcDr,wobDl)));
		res.pf.q3 = le.pf.q3 * (re.pf.q3 * mk_pf(min( wcDr,wobDl)) + re.pf.q1 * mk_pf(min( wcDr, wcDl))) +
			    le.pf.q4 * (re.pf.q3 * mk_pf(min(wobDr,wobDl)) + re.pf.q1 * mk_pf(min(wobDr, wcDl)));
		res.pf.q4 = le.pf.q4 * (re.pf.q4 * mk_pf(min(wobDr,wobDl)) + re.pf.q2 * mk_pf(min(wobDr, wcDl))) +
			    le.pf.q3 * (re.pf.q4 * mk_pf(min( wcDr,wobDl)) + re.pf.q2 * mk_pf(min( wcDr, wcDl)));
		
		res.pf.q1 = res.pf.q1 * scale(1);
		res.pf.q2 = res.pf.q2 * scale(1);
		res.pf.q3 = res.pf.q3 * scale(1);
		res.pf.q4 = res.pf.q4 * scale(1);
		
		res.subword.i = le.subword.i;
		res.subword.j = re.subword.j;
		
		return res;
	}

	choice [pfanswer] h([pfanswer] i) {
		return list(sum(i));
		//~ return i;
	}
}



algebra mfe implements Canonical_Algebra(alphabet = char, answer = mfeanswer) {
	mfeanswer sadd(Subsequence lb,mfeanswer e) {
		mfeanswer res;
		res.energy = e.energy;
		res.firstStem.seq = lb.seq;
		res.firstStem.i = lb.i;
		res.firstStem.j = e.firstStem.j;
		
		string o;
		append(o, "sadd{", 5);
		append(o, e.firstStem);
		append(o, e.rep);
		append(o, "}",1);
		res.rep = o;
		return res;
	}

	mfeanswer cadd(mfeanswer le,mfeanswer re) {
		mfeanswer res;
		res.energy = le.energy + re.energy;
		res.firstStem = le.firstStem;
		
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
		res.firstStem = le.firstStem;
		
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
		res.firstStem = le.firstStem;
		
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
		res.firstStem = le.firstStem;
		
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
		res.energy = le.energy + re.energy + min(dr_energy(le.firstStem, le.firstStem), dl_energy(re.firstStem, re.firstStem));
		res.firstStem = le.firstStem;
		
		string o1;
		string o2;
		append(o1, "ambd{", 5);
		append(o1, le.rep);
		append(o1, ",", 1);
		append(o1, re.rep);
		append(o1, ",min(dr_energy(", 15);
		append(o2, dr_energy(le.firstStem, le.firstStem));
		append(o2, "),dl_energy(", 12);
		append(o2, ")=",2);
		append(o2, min(dr_energy(le.firstStem, le.firstStem), dl_energy(re.firstStem, re.firstStem)));
		append(o2, ")}", 2);
		string o;
		append(o,o1);
		append(o,o2);
		res.rep = o;
		return res;
	}

	mfeanswer ambd_Pr(mfeanswer le,Subsequence b,mfeanswer re) {
		mfeanswer res;
		res.energy = le.energy + re.energy + min(dr_energy(le.firstStem, le.firstStem), dl_energy(re.firstStem, re.firstStem));
		res.firstStem = le.firstStem;
		
		string o1;
		string o2;
		append(o1, "ambd'{", 5);
		append(o1, le.rep);
		append(o1, ",", 1);
		append(o1, re.rep);
		append(o1, ",min(dr_energy(", 15);
		append(o2, dr_energy(le.firstStem, le.firstStem));
		append(o2, "),dl_energy(", 12);
		append(o2, ")=",2);
		append(o2, min(dr_energy(le.firstStem, le.firstStem), dl_energy(re.firstStem, re.firstStem)));
		append(o2, ")}", 2);
		string o;
		append(o,o1);
		append(o,o2);
		res.rep = o;
		return res;
	}

	mfeanswer nil(Subsequence loc) {
		mfeanswer res;
		res.energy = 0;
		res.firstStem = loc;
		
		string o;
		append(o, "nil{", 4);
		append(o, res.firstStem);
		append(o, "0}", 2);
		res.rep = o;
		return res;
	}

	mfeanswer nil_Pr(Subsequence loc) {
		mfeanswer res;
		res.energy = 0;
		res.firstStem = loc;
		
		string o;
		append(o, "nil'{", 5);
		append(o, res.firstStem);
		append(o, "0}", 2);
		res.rep = o;
		return res;
	}

	mfeanswer edl(Subsequence lb,mfeanswer e) {
		mfeanswer res;
		res.energy = e.energy + dl_energy(e.firstStem, e.firstStem);
		res.firstStem = e.firstStem;
		
		string o;
		append(o, "edl{", 4);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, dl_energy(e.firstStem, e.firstStem));
		append(o, "}", 1);
		res.rep = o;
		return res;
	}

	mfeanswer edr(mfeanswer e,Subsequence rb) {
		mfeanswer res;
		res.energy = e.energy + dr_energy(e.firstStem, e.firstStem);
		res.firstStem = e.firstStem;
		
		string o;
		append(o, "edr{", 4);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, dr_energy(e.firstStem, e.firstStem));
		append(o, "}", 1);
		res.rep = o;
		return res;
	}

	mfeanswer edlr(Subsequence lb,mfeanswer e,Subsequence rb) {
		mfeanswer res;
		res.energy = e.energy + dl_energy(e.firstStem, e.firstStem) + dr_energy(e.firstStem, e.firstStem);
		res.firstStem = e.firstStem;
		
		string o;
		append(o, "edlr{", 5);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, dl_energy(e.firstStem, e.firstStem) + dr_energy(e.firstStem, e.firstStem));
		append(o, "}", 1);
		res.rep = o;
		return res;
	}

	mfeanswer drem(mfeanswer e) {
		mfeanswer res;
		res = e;
		res.firstStem = e.firstStem;
		
		string o;
		append(o, "drem{", 5);
		append(o, e.rep);
		append(o, "}", 1);
		res.rep = o;
		return res;
	}

	mfeanswer is(mfeanswer e) {
		mfeanswer res;
		res.energy = e.energy + termau_energy(e.firstStem, e.firstStem);
		res.firstStem = e.firstStem;
		
		string o;
		append(o, "is{", 3);
		append(o, e.rep);
		append(o, ",termau_energy(",15);
		append(o, termau_energy(e.firstStem, e.firstStem));
		append(o, ")}", 2);
		res.rep = o;
		return res;
	}

	mfeanswer sr(Subsequence lb,mfeanswer e,Subsequence rb) {
		mfeanswer res;
		res.firstStem.seq = lb.seq;
		res.firstStem.i = lb.i;
		res.firstStem.j = rb.j;
		
		res.energy = e.energy + sr_energy(res.firstStem,res.firstStem);
		
		string o;
		append(o, "sr{", 3);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, sr_energy(res.firstStem,res.firstStem));
		append(o, "}", 1);
		res.rep = o;
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
		
		res.energy = hl_energy(innerStem, innerStem) + sr_energy(res.firstStem,res.firstStem);
		
		string o;
		append(o, "hl{", 3);
		append(o, hl_energy(innerStem, innerStem) + sr_energy(res.firstStem,res.firstStem));
		append(o, "}", 1);
		res.rep = o;
		return res;
	}

	mfeanswer sp(Subsequence llb,Subsequence lb,mfeanswer e,Subsequence rb,Subsequence rrb) {
		mfeanswer res;
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		res.energy = e.energy + sr_energy(res.firstStem,res.firstStem);
		
		string o;
		append(o, "sp{", 3);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, sr_energy(res.firstStem,res.firstStem));
		append(o, "}", 1);
		res.rep = o;
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
		
		res.energy = e.energy + bl_energy(innerStem,lregion,innerStem);
		
		string o;
		append(o, "bl{", 3);
		append(o, lregion);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, bl_energy(innerStem,lregion,innerStem));
		append(o, "}", 1);
		res.rep = o;
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
		
		res.energy = e.energy + br_energy(innerStem, rregion, innerStem);  
		
		string o;
		append(o, "br{", 3);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, br_energy(innerStem, rregion, innerStem));
		append(o, "}", 1);
		res.rep = o;
		return res;
	}

	mfeanswer il(Subsequence lregion,mfeanswer e,Subsequence rregion) {
		mfeanswer res;
		res.firstStem.seq = lregion.seq;
		res.firstStem.i = lregion.i;
		res.firstStem.j = rregion.j;
		
		res.energy = e.energy + il_energy(lregion, rregion);
		
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
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = ml_energy() + ul_energy() + e.energy + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem);
		
		string o;
		append(o, "ml{", 3);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, ml_energy() + ul_energy()+sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem));
		append(o, "}", 1);
		res.rep = o;
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
		
		string o;
		append(o, "mldr{", 5);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, ml_energy() + ul_energy()+dri_energy(innerStem,innerStem) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem));
		append(o, "}", 1);
		res.rep = o;
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
		
		string o1;
		string o2;
		string o3;
		append(o1, "mladr{ml_energy() + ul_energy(),", 10);
		append(o1, e.rep);
		append(o1, ",", 1);
		append(o1, "min(dri_energy(", 15);
		append(o1, dri_energy(innerStem,innerStem));
		append(o2, "),dr_energy(", 12);
		append(o2, dr_energy(e.lastStem, e.lastStem));
		append(o2, ")=",2);
		append(o2, min(dri_energy(innerStem,innerStem), dr_energy(e.lastStem, e.lastStem)));
		append(o3, "),sr_energy(",12);
		append(o3, sr_energy(res.firstStem,res.firstStem));
		append(o3, "), termau_energy(", 16);
		append(o3, termau_energy(innerStem,innerStem));
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
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = ml_energy() + ul_energy() + e.energy + dli_energy(innerStem,innerStem) + dri_energy(innerStem,innerStem) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem);
		
		string o;
		append(o, "mldlr{", 6);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, ml_energy() + ul_energy()+dli_energy(innerStem,innerStem) + dri_energy(innerStem,innerStem) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem));
		append(o, "}", 1);
		res.rep = o;
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
		
		string o1;
		string o2;
		string o3;
		string o4;
		string o5;
		append(o1, "mladlr{ml_energy() + ul_energy(),", 11);
		append(o1, e.rep);
		append(o1, ",min(dli_energy(",16);
		append(o1, dli_energy(innerStem,innerStem));
		append(o2, "),dl_energy(",12);
		append(o2, dl_energy(e.firstStem, e.firstStem));
		append(o2, "=",1);
		append(o2, min(dli_energy(innerStem,innerStem), dr_energy(e.lastStem, e.lastStem)));
		append(o3, "),min(dri_energy(",17);
		append(o3, dri_energy(innerStem,innerStem));
		append(o3, "),dl_energy(",12);
		append(o3, dr_energy(e.lastStem, e.lastStem));
		append(o4, "=",1);
		append(o4, min(dri_energy(innerStem,innerStem), dr_energy(e.lastStem, e.lastStem)));
		append(o4, "),sr_energy(",12);
		append(o4, sr_energy(res.firstStem,res.firstStem));
		append(o5, "),termau_energy(",16);
		append(o5, termau_energy(innerStem,innerStem));
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
		res.firstStem.seq = llb.seq;
		res.firstStem.i = llb.i;
		res.firstStem.j = rrb.j;
		
		Subsequence innerStem;
		innerStem.seq = lb.seq;
		innerStem.i = lb.i;
		innerStem.j = rb.j;
		
		res.energy = ml_energy() + ul_energy() + e.energy + dli_energy(innerStem,innerStem) + min(dri_energy(innerStem,innerStem), dr_energy(e.lastStem,e.lastStem)) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem);
		
		string o1;
		string o2;
		string o3;
		string o4;
		append(o1, "mldladr{ml_energy() + ul_energy(), ", 13);
		append(o1, e.rep);
		append(o1, "), dli_energy(", 14);
		append(o2, dli_energy(innerStem,innerStem));
		append(o2, "), min(dri_energy(", 18);
		append(o2, dri_energy(innerStem,innerStem));
		append(o2, "), dr_energy(", 13);
		append(o3, dr_energy(e.lastStem,e.lastStem));
		append(o3, "))=", 3);
		append(o3, min(dri_energy(innerStem,innerStem), dr_energy(e.lastStem,e.lastStem)));
		append(o3, ", sr_energy(", 12);
		append(o4, sr_energy(res.firstStem,res.firstStem));
		append(o4, "), termau_energy(", 17);
		append(o4, termau_energy(innerStem,innerStem));
		append(o4, ")}", 2);
		string o;
		append(o, o1);
		append(o, o2);
		append(o, o3);
		append(o, o4);
		res.rep = o;
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
		
		string o;
		append(o, "mladldr{", 8);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, ml_energy() + ul_energy()+min(dli_energy(innerStem,innerStem), dl_energy(e.firstStem, e.firstStem)) + dri_energy(innerStem,innerStem) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem));
		append(o, "}", 1);
		res.rep = o;
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
		
		string o;
		append(o, "mldl{", 5);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, ml_energy() + ul_energy()+dli_energy(innerStem,innerStem) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem));
		append(o, "}", 1);
		res.rep = o;
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
		
		string o;
		append(o, "mladl{", 6);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, ml_energy() + ul_energy()+min(dli_energy(innerStem,innerStem), dl_energy(e.firstStem, e.firstStem)) + sr_energy(res.firstStem,res.firstStem) + termau_energy(innerStem,innerStem));
		append(o, "}", 1);
		res.rep = o;
		return res;
	}

	mfeanswer addss(mfeanswer e,Subsequence rb) {
		mfeanswer res;
		res.energy = e.energy + ss_energy(rb);
		
		res.firstStem = e.firstStem;
		res.lastStem = e.lastStem;
		
		string o;
		append(o, "addss{", 6);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, ss_energy(rb));
		append(o, "}", 1);
		res.rep = o;
		return res;
	}

	mfeanswer ssadd(Subsequence lb,mfeanswer e) {
		mfeanswer res;
		res.energy = ul_energy() + e.energy + ss_energy(lb);
		
		res.firstStem = e.firstStem;
		res.lastStem = e.firstStem;
		
		string o;
		append(o, "ssadd{ul_energy(),", 9);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, ss_energy(lb));
		append(o, "}", 1);
		res.rep = o;
		return res;
	}

	mfeanswer trafo(mfeanswer e) {
		mfeanswer res;
		res = e;
		
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
		
		res.firstStem = e.firstStem;
		res.lastStem = e.firstStem;
		
		string o;
		append(o, "incl{", 5);
		append(o, e.rep);
		append(o, ",", 1);
		append(o, "ul_energy()}", 3);
		res.rep = o;
		return res;
	}

	mfeanswer combine(mfeanswer le,mfeanswer re) {
		mfeanswer res;
		res.energy = le.energy + re.energy;
		
		res.firstStem = le.firstStem;
		res.lastStem = re.lastStem;
		
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
		res.energy = le.energy + re.energy + min(dr_energy(le.lastStem, le.lastStem), dl_energy(re.firstStem, re.firstStem));
		res.firstStem = le.firstStem;
		res.lastStem = re.lastStem;
		
		string o1;
		string o2;
		string o3;
		append(o1, "acomb{", 6);
		append(o1, le.rep);
		append(o2, ",", 1);
		append(o2, re.rep);
		append(o2, ",min(dr_energy(", 15);
		append(o2, dr_energy(le.lastStem, le.lastStem));
		append(o3, "),dl_energy(", 12);
		append(o3, dl_energy(re.firstStem, re.firstStem));
		append(o3, ")=", 2);
		append(o3, min(dr_energy(le.lastStem, le.lastStem), dl_energy(re.firstStem, re.firstStem)));
		append(o3, "}", 1);
		string o;
		append(o, o1);
		append(o, o2);
		append(o, o3);
		res.rep = o;
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
		shape_t res;
		shape_t emptyShape;

		if (e == emptyShape) {
			append(res, '_');
		} else {
			res = e;
		}

		return res;
	}

	shape_t cadd(shape_t le,shape_t re) {	
		shape_t unpaired;
		append(unpaired, '_');
		
		shape_t res;
		append(res, le);
		if (re != unpaired) append(res, re);

		return res;
	}

	shape_t cadd_Pr(shape_t le,shape_t re) {
		shape_t res;
		append(res, le);
		append(res, re);
		return res;
	}

	shape_t cadd_Pr_Pr(shape_t le,shape_t re) {
		shape_t unpaired;
		append(unpaired, '_');
		
		shape_t res;
		append(res, le);
		if (re != unpaired) append(res, re);

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

instance enumi = canonicals_nonamb ( enumi ) ;
instance count = canonicals_nonamb ( count ) ;
instance pf = canonicals_nonamb ( p_func ) ;
instance pppf = canonicals_nonamb ( pretty * p_func * enumi) ;
instance mfe = canonicals_nonamb ( mfe ) ;
instance ppmfe = canonicals_nonamb ( pretty * mfe ) ;
instance mfepp = canonicals_nonamb ( mfe * pretty ) ;
instance pretty = canonicals_nonamb ( pretty ) ;
instance shape5 = canonicals_nonamb ( shape5 ) ;
instance shape5pf = canonicals_nonamb (shape5 * p_func);

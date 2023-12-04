import rules

input raw

type Rope = extern
type rules = extern

signature sig_TDM(alphabet, answer, output) {
  output convert(answer);
  answer root(answer);
  answer unpaired(alphabet);
  answer last_hlmode(answer);
  answer next_hlmode(answer, answer);
  answer last_mlmode(answer, answer);
  answer next_mlmode(answer, answer);
  answer hairpin(alphabet, alphabet);
  answer multiloop(alphabet, answer, alphabet);
  answer dangle(answer);
  answer strong(answer);
  answer helixinterrupt(alphabet, answer, alphabet);
  answer internalloop(alphabet, alphabet, answer, alphabet, alphabet);
  answer leftbulge(alphabet, alphabet, answer, alphabet);
  answer rightbulge(alphabet, answer, alphabet, alphabet);
  choice [answer] h([answer]);
}

algebra alg_count auto count;
algebra alg_enum auto enum;

algebra alg_tdm_overdangle_5 implements sig_TDM(alphabet = char, answer = rules, output = Rope) {
  Rope convert(rules x) {
	return "grammar gra_overdangle uses sig_foldrna(axiom = struct) {\n" + toRope(x) + "}\n";
  }
  rules root(rules x) {
	rules res = x;
	insertProduction(res, "struct", "struct__"+x.shape);
	insertProduction(res, "struct___", "sadd(BASE, struct___)");
	insertProduction(res, "struct___", "nil(LOC)");
    return res;
  }
  rules unpaired(alphabet a) {
	rules res;
	setShape(res, "_");
	insertProduction(res, "struct", "struct__"+res.shape);
	insertProduction(res, "struct__"+res.shape, "sadd(BASE, struct__"+res.shape+")");
	insertProduction(res, "struct__"+res.shape, "nil(LOC)");
    return res;
  }
  rules last_hlmode(rules x) {
	rules res = x;
	insertProduction(res, "struct__"+x.shape, "sadd(BASE, struct__"+x.shape+")");
	insertProduction(res, "struct__"+x.shape, "cadd(dangle__"+x.shape+", struct___)");
	return res;
  }
  rules next_hlmode(rules x, rules y) {
	rules res = x + y;
	insertProduction(res, "struct__"+res.shape, "sadd(BASE, struct__"+res.shape+")");
	insertProduction(res, "struct__"+res.shape, "cadd(dangle__"+x.shape+", struct__"+y.shape+")");
	return res;
  }
  rules last_mlmode(rules x, rules y) {
	rules res = x + y;
	insertProduction(res, "ml_comps__"+y.shape, "sadd(BASE, ml_comps__"+y.shape+")");
	insertProduction(res, "ml_comps__"+y.shape, "incl(dangle__"+y.shape+")");
	insertProduction(res, "ml_comps__"+y.shape, "addss(incl(dangle__"+y.shape+"), REGION)");
	insertProduction(res, "ml_comps__"+res.shape, "sadd(BASE, ml_comps__"+res.shape+")");
	insertProduction(res, "ml_comps__"+res.shape, "cadd(incl(dangle__"+x.shape+"), ml_comps__"+y.shape+")");
    return res;
  }
  rules next_mlmode(rules x, rules y) {
    rules res = x + y;
	insertProduction(res, "ml_comps__"+res.shape, "sadd(BASE, ml_comps__"+res.shape+")");
	insertProduction(res, "ml_comps__"+res.shape, "cadd(incl(dangle__"+x.shape+"), ml_comps__"+y.shape+")");
	return res;
  }
  rules hairpin(alphabet a, alphabet b) {
	rules res;
	setShape(res, "LJ");
	  
	insertProduction(res, "weak__"+res.shape, "hairpin__"+res.shape);
	insertProduction(res, "hairpin__"+res.shape, "hl(BASE, REGION with minsize(3), BASE) with basepair");
	  
    return res;
  }
  rules multiloop(alphabet a, rules x, alphabet b) {
	rules res = x;
	setShape(res, "L"+x.shape+"J");
	  
	insertProduction(res, "weak__"+res.shape, "multiloop__"+res.shape);
	insertProduction(res, "multiloop__"+res.shape, "ml(BASE, ml_comps__"+x.shape+", BASE) with basepair");
	  
    return res;
  }
  rules dangle(rules x) {
	rules res = x;
	  
	insertProduction(res, "dangle__"+res.shape, "drem(LOC, strong__"+res.shape+",LOC)");

	return res;
  }
  rules strong(rules x) {
	rules res = x;
	  
	insertProduction(res, "strong__"+res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");  
	
	insertProduction(res, "weak__"+res.shape, "stack__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "leftB__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "rightB__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "iloop__"+res.shape);
	  
	insertProduction(res, "stack__" +res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");
	insertProduction(res, "leftB__" +res.shape, "bl(BASE, REGION with maxsize(30), strong__"+res.shape+", BASE) with basepair");
	insertProduction(res, "rightB__"+res.shape, "br(BASE, strong__"+res.shape+", REGION with maxsize(30), BASE) with basepair");
	insertProduction(res, "iloop__" +res.shape, "il(BASE, REGION with maxsize(30), strong__"+res.shape+", REGION with maxsize(30), BASE) with basepair");

	return res;
  }
  
  rules helixinterrupt(alphabet a, rules x, alphabet b) { return x; } // this function does only appear in shape levels 4 and 3!
  rules internalloop(alphabet a, alphabet b, rules x, alphabet c, alphabet d) { return x; } // this function does only appear in shape level 2!
  rules leftbulge(alphabet a, alphabet b, rules x, alphabet d) { return x; } // this function does only appear in shape level 2!
  rules rightbulge(alphabet a, rules x, alphabet c, alphabet d) { return x; } // this function does only appear in shape level 2!
  
  choice [rules] h([rules] i) {
	return i;
  }
}

algebra alg_tdm_overdangle_4 extends alg_tdm_overdangle_5 {
  rules strong(rules x) {
	rules res = x;
	  
	insertProduction(res, "strong__"+res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");  
	
	insertProduction(res, "weak__"+res.shape, "stack__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "leftB__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "rightB__"+res.shape);
	  
	insertProduction(res, "stack__" +res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");
	insertProduction(res, "leftB__" +res.shape, "bl(BASE, REGION with maxsize(30), strong__"+res.shape+", BASE) with basepair");
	insertProduction(res, "rightB__"+res.shape, "br(BASE, strong__"+res.shape+", REGION with maxsize(30), BASE) with basepair");

	return res;
  }
  rules helixinterrupt(alphabet a, rules x, alphabet b) {
	rules res = x;
	setShape(res, "L"+x.shape+"J");
	  
	insertProduction(res, "weak__"+res.shape, "iloop__"+res.shape);
	insertProduction(res, "iloop__" +res.shape, "il(BASE, REGION with maxsize(30), strong__"+x.shape+", REGION with maxsize(30), BASE) with basepair");
	
	return res;
  }
}

algebra alg_tdm_overdangle_3 extends alg_tdm_overdangle_5 {
  rules strong(rules x) {
	rules res = x;
	  
	insertProduction(res, "strong__"+res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");  
	insertProduction(res, "weak__"+res.shape, "stack__"+res.shape);
	insertProduction(res, "stack__" +res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");

	return res;
  }
  rules helixinterrupt(alphabet a, rules x, alphabet b) {
	rules res = x;
	setShape(res, "L"+x.shape+"J");
	  
	insertProduction(res, "weak__"+res.shape, "iloop__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "leftB__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "rightB__"+res.shape);
	  
	insertProduction(res, "iloop__" +res.shape, "il(BASE, REGION with maxsize(30), strong__"+x.shape+", REGION with maxsize(30), BASE) with basepair");
	insertProduction(res, "leftB__" +res.shape, "bl(BASE, REGION with maxsize(30), strong__"+x.shape+", BASE) with basepair");
	insertProduction(res, "rightB__"+res.shape, "br(BASE, strong__"+x.shape+", REGION with maxsize(30), BASE) with basepair");
	
	return res;
  }
}

algebra alg_tdm_overdangle_2 extends alg_tdm_overdangle_5 {
  rules strong(rules x) {
	rules res = x;
	insertProduction(res, "strong__"+res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");  
	insertProduction(res, "weak__"+res.shape, "stack__"+res.shape);
	insertProduction(res, "stack__" +res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");
	return res;
  }
  rules internalloop(alphabet a, alphabet b, rules x, alphabet c, alphabet d) { 
	rules res = x;
	setShape(res, "L_"+x.shape+"_J");
	insertProduction(res, "weak__"+res.shape, "iloop__"+res.shape);
	insertProduction(res, "iloop__" +res.shape, "il(BASE, REGION with maxsize(30), strong__"+x.shape+", REGION with maxsize(30), BASE) with basepair");
	return res;
  }
  rules leftbulge(alphabet a, alphabet b, rules x, alphabet d) { 
	rules res = x;
	setShape(res, "L_"+x.shape+"J");
	insertProduction(res, "weak__"+res.shape, "leftB__"+res.shape);
	insertProduction(res, "leftB__" +res.shape, "bl(BASE, REGION with maxsize(30), strong__"+x.shape+", BASE) with basepair");
 	return res;
 }
  rules rightbulge(alphabet a, rules x, alphabet c, alphabet d) { 
	rules res = x;
	setShape(res, "L"+x.shape+"_J");
	insertProduction(res, "weak__"+res.shape, "rightB__"+res.shape);
	insertProduction(res, "rightB__"+res.shape, "br(BASE, strong__"+x.shape+", REGION with maxsize(30), BASE) with basepair");
	return res;
  }
}


algebra alg_tdm_macrostate_5 implements sig_TDM(alphabet = char, answer = rules, output = Rope) {
  Rope convert(rules x) {
	return "grammar gra_macrostate uses sig_foldrna(axiom = struct) {\n" + toRope(x) + "}\n";
  }
  rules root(rules x) {
	rules res = x;
	insertProduction(res, "struct", "left_dangle__"+x.shape);
	insertProduction(res, "struct", "trafo(noleft_dangle__"+x.shape+")");
	insertProduction(res, "struct", "left_unpaired__"+x.shape);
    return res;
  }
  rules unpaired(alphabet a) {
	rules res;
	setShape(res, "_");
    insertProduction(res, "struct", "nil(LOC)");
	insertProduction(res, "struct", "left_unpairedEnd");
	insertProduction(res, "left_unpairedEnd", "sadd(BASE, left_unpairedEnd)");
	insertProduction(res, "left_unpairedEnd", "sadd(BASE, nil(LOC))");

	return res;
  }
  rules last_hlmode(rules x) {
	rules res = x;
	insertProduction(res, "left_unpaired__"+x.shape, "sadd(BASE, left_unpaired__"+x.shape+")");
	insertProduction(res, "left_unpaired__"+x.shape, "sadd(BASE, left_dangle__"+x.shape+")");
	insertProduction(res, "left_dangle__"+x.shape, "cadd_Pr(edanglel__"+x.shape+", nil(LOC))");
	insertProduction(res, "left_dangle__"+x.shape, "cadd(edanglelr__"+x.shape+", {nil(LOC) | left_unpairedEnd})");
	insertProduction(res, "noleft_dangle__"+x.shape, "cadd_Pr_Pr(edangler__"+x.shape+", {nil(LOC) | left_unpairedEnd})");  
	insertProduction(res, "noleft_dangle__"+x.shape, "cadd_Pr_Pr_Pr(nodangle__"+x.shape+", nil(LOC))");
	  
	return res;
  }
  rules next_hlmode(rules x, rules y) {
	rules res = x + y;
	insertProduction(res, "left_unpaired__"+res.shape, "sadd(BASE, left_unpaired__"+res.shape+")");
	insertProduction(res, "left_unpaired__"+res.shape, "sadd(BASE, left_dangle__"+res.shape+")");
	insertProduction(res, "left_dangle__"+res.shape, "ambd(edanglel__"+x.shape+", BASE, noleft_dangle__"+y.shape+")");
	insertProduction(res, "left_dangle__"+res.shape, "cadd_Pr(edanglel__"+x.shape+", noleft_dangle__"+y.shape+")");
	insertProduction(res, "left_dangle__"+res.shape, "cadd(edanglelr__"+x.shape+", {left_dangle__"+y.shape+" | left_unpaired__"+y.shape+"})");
	insertProduction(res, "noleft_dangle__"+res.shape, "cadd_Pr_Pr(edangler__"+x.shape+", {left_dangle__"+y.shape+" | left_unpaired__"+y.shape+"})");  
	insertProduction(res, "noleft_dangle__"+res.shape, "cadd_Pr_Pr_Pr(nodangle__"+x.shape+", noleft_dangle__"+y.shape+")");
	insertProduction(res, "noleft_dangle__"+res.shape, "ambd_Pr(nodangle__"+x.shape+", BASE, noleft_dangle__"+y.shape+")");
	  
	return res;
  }
  rules last_mlmode(rules x, rules y) {
	rules res = x + y;
	  
	insertProduction(res, "ml_comps1__"+res.shape, "combine(block_dl__"+x.shape+", no_dl_no_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps1__"+res.shape, "combine(block_dlr__"+x.shape+", dl_or_ss_left_no_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps1__"+res.shape, "acomb(block_dl__"+x.shape+", BASE, no_dl_no_ss_end__"+y.shape+")");
	
	insertProduction(res, "ml_comps2__"+res.shape, "combine(incl(nodangle__"+x.shape+"), no_dl_no_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps2__"+res.shape, "combine(incl(edangler__"+x.shape+"), dl_or_ss_left_no_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps2__"+res.shape, "acomb(incl(nodangle__"+x.shape+"), BASE, no_dl_no_ss_end__"+y.shape+")");
	
	insertProduction(res, "ml_comps3__"+res.shape, "combine(incl(edangler__"+x.shape+"), dl_or_ss_left_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps3__"+res.shape, "combine(incl(nodangle__"+x.shape+"), no_dl_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps3__"+res.shape, "acomb(incl(nodangle__"+x.shape+"), BASE, no_dl_ss_end__"+y.shape+")");
	
	insertProduction(res, "ml_comps4__"+res.shape, "combine(block_dl__"+x.shape+", no_dl_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps4__"+res.shape, "combine(block_dlr__"+x.shape+", dl_or_ss_left_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps4__"+res.shape, "acomb(block_dl__"+x.shape+", BASE, no_dl_ss_end__"+y.shape+")");
	  
	insertProduction(res, "no_dl_no_ss_end__"+y.shape, "incl(nodangle__"+y.shape+")");
	insertProduction(res, "dl_or_ss_left_no_ss_end__"+y.shape, "block_dl__"+y.shape);
	insertProduction(res, "no_dl_ss_end__"+y.shape, "incl(edangler__"+y.shape+")");
	insertProduction(res, "no_dl_ss_end__"+y.shape, "addss(incl(edangler__"+y.shape+"), REGION)");
	insertProduction(res, "dl_or_ss_left_ss_end__"+y.shape, "block_dlr__"+y.shape);
	insertProduction(res, "dl_or_ss_left_ss_end__"+y.shape, "addss(block_dlr__"+y.shape+", REGION)");

    insertProduction(res, "block_dl__"+x.shape, "sadd(REGION, edanglel__"+x.shape+")");
    insertProduction(res, "block_dl__"+x.shape, "incl(edanglel__"+x.shape+")");
	insertProduction(res, "block_dlr__"+x.shape, "ssadd(REGION, edanglelr__"+x.shape+")");
	insertProduction(res, "block_dlr__"+x.shape, "incl(edanglelr__"+x.shape+")");
	
	insertProduction(res, "block_dl__"+y.shape, "sadd(REGION, edanglel__"+y.shape+")");
    insertProduction(res, "block_dl__"+y.shape, "incl(edanglel__"+y.shape+")");
	insertProduction(res, "block_dlr__"+y.shape, "ssadd(REGION, edanglelr__"+y.shape+")");
	insertProduction(res, "block_dlr__"+y.shape, "incl(edanglelr__"+y.shape+")");
	
    return res;
  }
  rules next_mlmode(rules x, rules y) {
    rules res = x + y;
	  
	insertProduction(res, "ml_comps1__"+res.shape, "combine(block_dl__"+x.shape+", no_dl_no_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps1__"+res.shape, "combine(block_dlr__"+x.shape+", dl_or_ss_left_no_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps1__"+res.shape, "acomb(block_dl__"+x.shape+", BASE, no_dl_no_ss_end__"+y.shape+")");
	
	insertProduction(res, "ml_comps2__"+res.shape, "combine(incl(nodangle__"+x.shape+"), no_dl_no_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps2__"+res.shape, "combine(incl(edangler__"+x.shape+"), dl_or_ss_left_no_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps2__"+res.shape, "acomb(incl(nodangle__"+x.shape+"), BASE, no_dl_no_ss_end__"+y.shape+")");
	
	insertProduction(res, "ml_comps3__"+res.shape, "combine(incl(edangler__"+x.shape+"), dl_or_ss_left_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps3__"+res.shape, "combine(incl(nodangle__"+x.shape+"), no_dl_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps3__"+res.shape, "acomb(incl(nodangle__"+x.shape+"), BASE, no_dl_ss_end__"+y.shape+")");
	
	insertProduction(res, "ml_comps4__"+res.shape, "combine(block_dl__"+x.shape+", no_dl_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps4__"+res.shape, "combine(block_dlr__"+x.shape+", dl_or_ss_left_ss_end__"+y.shape+")");
	insertProduction(res, "ml_comps4__"+res.shape, "acomb(block_dl__"+x.shape+", BASE, no_dl_ss_end__"+y.shape+")");

    insertProduction(res, "no_dl_no_ss_end__"+y.shape, "ml_comps2__"+y.shape);
	insertProduction(res, "dl_or_ss_left_no_ss_end__"+y.shape, "ml_comps1__"+y.shape);
	insertProduction(res, "no_dl_ss_end__"+y.shape, "ml_comps3__"+y.shape);
	insertProduction(res, "dl_or_ss_left_ss_end__"+y.shape, "ml_comps4__"+y.shape);
	  
    insertProduction(res, "block_dl__"+x.shape, "sadd(REGION, edanglel__"+x.shape+")");
    insertProduction(res, "block_dl__"+x.shape, "incl(edanglel__"+x.shape+")");
	insertProduction(res, "block_dlr__"+x.shape, "ssadd(REGION, edanglelr__"+x.shape+")");
	insertProduction(res, "block_dlr__"+x.shape, "incl(edanglelr__"+x.shape+")");
	
	return res;
  }
  rules hairpin(alphabet a, alphabet b) {
	rules res;
	setShape(res, "LJ");
	  
	insertProduction(res, "weak__"+res.shape, "hairpin__"+res.shape);
	insertProduction(res, "hairpin__"+res.shape, "hl(BASE, REGION with minsize(3), BASE) with basepair");
	  
    return res;
  }
  rules multiloop(alphabet a, rules x, alphabet b) {
	rules res = x;
	setShape(res, "L"+x.shape+"J");
	  
	insertProduction(res, "weak__"+res.shape, "multiloop__"+res.shape);
	insertProduction(res, "multiloop__"+res.shape, "mldl(BASE, BASE, ml_comps1__"+x.shape+", BASE) with basepair");
	insertProduction(res, "multiloop__"+res.shape, "mladl(BASE, BASE, ml_comps2__"+x.shape+", BASE) with basepair");
	insertProduction(res, "multiloop__"+res.shape, "mldr(BASE, ml_comps3__"+x.shape+", BASE, BASE) with basepair");
	insertProduction(res, "multiloop__"+res.shape, "mladr(BASE, ml_comps2__"+x.shape+", BASE, BASE) with basepair");
	insertProduction(res, "multiloop__"+res.shape, "mldlr(BASE, BASE, ml_comps4__"+x.shape+", BASE, BASE) with basepair");
	insertProduction(res, "multiloop__"+res.shape, "mladlr(BASE, BASE, ml_comps2__"+x.shape+", BASE, BASE) with basepair");
	insertProduction(res, "multiloop__"+res.shape, "mldladr(BASE, BASE, ml_comps1__"+x.shape+", BASE, BASE) with basepair");
	insertProduction(res, "multiloop__"+res.shape, "mladldr(BASE, BASE, ml_comps3__"+x.shape+", BASE, BASE) with basepair");
	insertProduction(res, "multiloop__"+res.shape, "ml(BASE, ml_comps2__"+x.shape+", BASE) with basepair");
	  
    return res;
  }
  rules dangle(rules x) {
	rules res = x;
	
	insertProduction(res, "edanglel__" +res.shape, "edl (BASE, strong__"+res.shape+", LOC )");
	insertProduction(res, "edangler__" +res.shape, "edr (LOC,  strong__"+res.shape+", BASE)");
	insertProduction(res, "edanglelr__"+res.shape, "edlr(BASE, strong__"+res.shape+", BASE)");
	insertProduction(res, "nodangle__" +res.shape, "drem(LOC,  strong__"+res.shape+", LOC )");
	  
	return res;
  }
  rules strong(rules x) {
	rules res = x;
	
	insertProduction(res, "strong__"+x.shape, "sr(BASE, weak__"+x.shape+", BASE) with basepair");
	insertProduction(res, "weak__"+res.shape, "stack__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "leftB__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "rightB__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "iloop__"+res.shape);

	insertProduction(res, "stack__"+res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");
	insertProduction(res, "leftB__"+res.shape, "bl(BASE, REGION with maxsize(30), strong__"+res.shape+", BASE) with basepair");
	insertProduction(res, "rightB__"+res.shape, "br(BASE, strong__"+res.shape+", REGION with maxsize(30), BASE) with basepair");
	insertProduction(res, "iloop__"+res.shape, "il(BASE, REGION with maxsize(30), strong__"+res.shape+", REGION with maxsize(30), BASE) with basepair");

	insertProduction(res, "stack__"+res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");
	insertProduction(res, "leftB__"+res.shape, "bl(BASE, REGION with maxsize(30), strong__"+res.shape+", BASE) with basepair");
	insertProduction(res, "rightB__"+res.shape, "br(BASE, strong__"+res.shape+", REGION with maxsize(30), BASE) with basepair");
	insertProduction(res, "iloop__"+res.shape, "il(BASE, REGION with maxsize(30), strong__"+res.shape+", REGION with maxsize(30), BASE) with basepair");

	insertProduction(res, "left_unpairedEnd", "sadd(BASE, left_unpairedEnd)");
	insertProduction(res, "left_unpairedEnd", "sadd(BASE, nil(LOC))");
	  
	return res;
  }
  
  rules helixinterrupt(alphabet a, rules x, alphabet b) { return x; } // this function does only appear in shape levels 4 and 3!
  rules internalloop(alphabet a, alphabet b, rules x, alphabet c, alphabet d) { return x; } // this function does only appear in shape level 2!
  rules leftbulge(alphabet a, alphabet b, rules x, alphabet d) { return x; } // this function does only appear in shape level 2!
  rules rightbulge(alphabet a, rules x, alphabet c, alphabet d) { return x; } // this function does only appear in shape level 2!
  
  choice [rules] h([rules] i) {
	return i;
  }
}

algebra alg_tdm_macrostate_4 extends alg_tdm_macrostate_5 {
  rules strong(rules x) {
	rules res = x;
	  
	insertProduction(res, "strong__"+res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");  
	
	insertProduction(res, "weak__"+res.shape, "stack__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "leftB__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "rightB__"+res.shape);
	  
	insertProduction(res, "stack__" +res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");
	insertProduction(res, "leftB__" +res.shape, "bl(BASE, REGION with maxsize(30), strong__"+res.shape+", BASE) with basepair");
	insertProduction(res, "rightB__"+res.shape, "br(BASE, strong__"+res.shape+", REGION with maxsize(30), BASE) with basepair");

	return res;
  }
  rules helixinterrupt(alphabet a, rules x, alphabet b) {
	rules res = x;
	setShape(res, "L"+x.shape+"J");
	  
	insertProduction(res, "weak__"+res.shape, "iloop__"+res.shape);
	insertProduction(res, "iloop__" +res.shape, "il(BASE, REGION with maxsize(30), strong__"+x.shape+", REGION with maxsize(30), BASE) with basepair");
	
	return res;
  }
}

algebra alg_tdm_macrostate_3 extends alg_tdm_macrostate_5 {
  rules strong(rules x) {
	rules res = x;
	  
	insertProduction(res, "strong__"+res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");  
	insertProduction(res, "weak__"+res.shape, "stack__"+res.shape);
	insertProduction(res, "stack__" +res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");

	return res;
  }
  rules helixinterrupt(alphabet a, rules x, alphabet b) {
	rules res = x;
	setShape(res, "L"+x.shape+"J");
	  
	insertProduction(res, "weak__"+res.shape, "iloop__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "leftB__"+res.shape);
	insertProduction(res, "weak__"+res.shape, "rightB__"+res.shape);
	  
	insertProduction(res, "iloop__" +res.shape, "il(BASE, REGION with maxsize(30), strong__"+x.shape+", REGION with maxsize(30), BASE) with basepair");
	insertProduction(res, "leftB__" +res.shape, "bl(BASE, REGION with maxsize(30), strong__"+x.shape+", BASE) with basepair");
	insertProduction(res, "rightB__"+res.shape, "br(BASE, strong__"+x.shape+", REGION with maxsize(30), BASE) with basepair");
	
	return res;
  }
}

algebra alg_tdm_macrostate_2 extends alg_tdm_macrostate_5 {
  rules strong(rules x) {
	rules res = x;
	insertProduction(res, "strong__"+res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");  
	insertProduction(res, "weak__"+res.shape, "stack__"+res.shape);
	insertProduction(res, "stack__" +res.shape, "sr(BASE, weak__"+res.shape+", BASE) with basepair");
	return res;
  }
  rules internalloop(alphabet a, alphabet b, rules x, alphabet c, alphabet d) { 
	rules res = x;
	setShape(res, "L_"+x.shape+"_J");
	insertProduction(res, "weak__"+res.shape, "iloop__"+res.shape);
	insertProduction(res, "iloop__" +res.shape, "il(BASE, REGION with maxsize(30), strong__"+x.shape+", REGION with maxsize(30), BASE) with basepair");
	return res;
  }
  rules leftbulge(alphabet a, alphabet b, rules x, alphabet d) { 
	rules res = x;
	setShape(res, "L_"+x.shape+"J");
	insertProduction(res, "weak__"+res.shape, "leftB__"+res.shape);
	insertProduction(res, "leftB__" +res.shape, "bl(BASE, REGION with maxsize(30), strong__"+x.shape+", BASE) with basepair");
 	return res;
 }
  rules rightbulge(alphabet a, rules x, alphabet c, alphabet d) { 
	rules res = x;
	setShape(res, "L"+x.shape+"_J");
	insertProduction(res, "weak__"+res.shape, "rightB__"+res.shape);
	insertProduction(res, "rightB__"+res.shape, "br(BASE, strong__"+x.shape+", REGION with maxsize(30), BASE) with basepair");
	return res;
  }
}

algebra alg_prettyprint implements sig_TDM(alphabet = char, answer = Rope, output = Rope) {
  Rope convert(Rope x) {
	return x;
  }
  Rope root(Rope x) {
	return x;
  }
  Rope unpaired(alphabet a) {
	return "_";
  }
  Rope last_hlmode(Rope x) {
	return x;
  }
  Rope next_hlmode(Rope x, Rope y) {
	return x + y;
  }
  Rope last_mlmode(Rope x, Rope y) {
	return x + y;
  }
  Rope next_mlmode(Rope x, Rope y) {
    return x + y;
  }
  Rope hairpin(alphabet a, alphabet b) {
	return "LJ";
  }
  Rope multiloop(alphabet a, Rope x, alphabet b) {
	return "L"+x+"J";
  }
  Rope dangle(Rope x) {
	return x;
  }
  Rope strong(Rope x) {
	return x;
  }
  
  Rope helixinterrupt(alphabet a, Rope x, alphabet b) { return x; } // this function does only appear in shape levels 4 and 3!
  Rope internalloop(alphabet a, alphabet b, Rope x, alphabet c, alphabet d) { return x; } // this function does only appear in shape level 2!
  Rope leftbulge(alphabet a, alphabet b, Rope x, alphabet d) { return x; } // this function does only appear in shape level 2!
  Rope rightbulge(alphabet a, Rope x, alphabet c, alphabet d) { return x; } // this function does only appear in shape level 2!
  
  choice [Rope] h([Rope] i) {
	return i;
  }
}

grammar gra_shape5 uses sig_TDM(axiom = head) {
  head        = convert(structure)                            ;

  structure   = unpaired(CHAR('_'))                           | 
                root(cons_hlmode)                             # h;
	
  cons_hlmode = last_hlmode(danglecomp)                       |
                next_hlmode(danglecomp, cons_hlmode)          # h;
                      
  cons_mlmode = last_mlmode(danglecomp, danglecomp)           |
                next_mlmode(danglecomp, cons_mlmode)          # h;
  
  danglecomp  = dangle(strong(comp))                          ;
   
  comp        = hairpin(CHAR('['),CHAR(']'))                  |
                multiloop(CHAR('['), cons_mlmode, CHAR(']'))  # h; 
}

grammar gra_shape4u3 uses sig_TDM(axiom = head) {
  head        = convert(structure)                                 ;
	                                        
  structure   = unpaired(CHAR('_'))                                | 
                root(cons_hlmode)                                  # h;
	     
  cons_hlmode = last_hlmode(danglecomp)                            |
                next_hlmode(danglecomp, cons_hlmode)               # h;
                           
  cons_mlmode = last_mlmode(danglecomp, danglecomp)                |
                next_mlmode(danglecomp, cons_mlmode)               # h;
       
  danglecomp  = dangle(strong(comp))                               ;
   
  comp        = hairpin(CHAR('['),CHAR(']'))                       |
	            helixinterrupt(CHAR('['), strong(comp), CHAR(']')) |
                multiloop(CHAR('['), cons_mlmode, CHAR(']'))       # h; 
}

grammar gra_shape2 uses sig_TDM(axiom = head) {
  head        = convert(structure)                                                      ;
	                                                           
  //~ ruleset     = structure                                                      ;
                        
  structure   = unpaired(CHAR('_'))                                                   | 
                root(cons_hlmode)                                                     # h;
	                        
  cons_hlmode = last_hlmode(danglecomp)                                               |
                next_hlmode(danglecomp, cons_hlmode)                                  # h;
                                              
  cons_mlmode = last_mlmode(danglecomp, danglecomp)                                   |
                next_mlmode(danglecomp, cons_mlmode)                                  # h;
                          
  danglecomp  = dangle(strong(comp))                                                  ;
   
  comp        = hairpin     (CHAR('['),                                    CHAR(']')) |
	            internalloop(CHAR('['),CHAR('_'), strong(comp), CHAR('_'), CHAR(']')) |
	            leftbulge   (CHAR('['),CHAR('_'), strong(comp),            CHAR(']')) |
	            rightbulge  (CHAR('['),           strong(comp), CHAR('_'), CHAR(']')) |
	            multiloop   (CHAR('['),           cons_mlmode,             CHAR(']')) # h; 
}

instance tdm_overdangle_5 = gra_shape5  (alg_tdm_overdangle_5);
instance tdm_overdangle_4 = gra_shape4u3(alg_tdm_overdangle_4);
instance tdm_overdangle_3 = gra_shape4u3(alg_tdm_overdangle_3);
instance tdm_overdangle_2 = gra_shape2  (alg_tdm_overdangle_2);

instance tdm_macrostate_5 = gra_shape5  (alg_tdm_macrostate_5);
instance tdm_macrostate_4 = gra_shape4u3(alg_tdm_macrostate_4);
instance tdm_macrostate_3 = gra_shape4u3(alg_tdm_macrostate_3);
instance tdm_macrostate_2 = gra_shape2  (alg_tdm_macrostate_2);




instance pp5 = gra_shape5  (alg_prettyprint);
instance pp4 = gra_shape4u3(alg_prettyprint);
instance pp3 = gra_shape4u3(alg_prettyprint);
instance pp2 = gra_shape2  (alg_prettyprint);
instance enum = gra_shape5(alg_enum);
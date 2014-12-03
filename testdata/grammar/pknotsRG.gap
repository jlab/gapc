import rna

import stacklen

import pkenergy

input rna

type shape_t = shape
// type base_t = extern // XXX
type Rope = extern
type mfeanswer = (int energy, int betaLeftOuter, int alphaRightOuter)
type string_t = Rope

type pkshape_t = extern
//type myShape = Rope
//type myShape = pkshape_t
type myShape = extern
type myBool = int

signature Algebra(alphabet, comp, compKnot) {
  comp sadd(Subsequence, comp);
  comp cadd(comp, comp);
  comp nil(void);
  comp is(Subsequence, comp, Subsequence);
  comp edl(Subsequence, comp, Subsequence);
  comp edr(Subsequence, comp, Subsequence);
  comp edlr(Subsequence, comp, Subsequence);
  comp pk(compKnot);
  compKnot pknot(Subsequence, comp, Subsequence, comp, Subsequence, comp, Subsequence ; int);
  compKnot pkiss(Subsequence, comp, Subsequence, comp, Subsequence, comp, Subsequence, comp, Subsequence, comp, Subsequence ; int);
  comp kndl(Subsequence, compKnot);
  comp kndr(compKnot, Subsequence);
  comp kndlr(Subsequence, compKnot, Subsequence);
  comp sr(Subsequence, comp, Subsequence);
  comp hl(Subsequence, Subsequence, Subsequence, Subsequence, Subsequence);
  comp bl(Subsequence, Subsequence, Subsequence, comp, Subsequence, Subsequence);
  comp br(Subsequence, Subsequence, comp, Subsequence, Subsequence, Subsequence);
  comp il(Subsequence, Subsequence, Subsequence, comp, Subsequence, Subsequence, Subsequence);
  comp ml(Subsequence, Subsequence, comp, Subsequence, Subsequence);
  comp mldl(Subsequence, Subsequence, Subsequence, comp, Subsequence, Subsequence);
  comp mldr(Subsequence, Subsequence, comp, Subsequence, Subsequence, Subsequence);
  comp mldlr(Subsequence, Subsequence, Subsequence, comp, Subsequence, Subsequence, Subsequence);
  comp addss(comp, Subsequence);
  comp mlstem(comp);
  comp pkml(comp);
  comp frd(comp, Subsequence; int);
  comp ul(comp);
  comp emptymid(Subsequence ; int, int);
  comp midbase(Subsequence ; int, int);
  comp middlro(Subsequence ; int, int);
  comp midregion(comp);
  comp middl(Subsequence, comp; int);
  comp middr(comp, Subsequence; int);
  comp middlr(Subsequence, comp, Subsequence; int, int);
  comp bkd(Subsequence, comp; int);
  comp pss(Subsequence);
  choice [comp] h([comp]);
  choice [compKnot] hKnot([compKnot]);
}


algebra mfe implements Algebra(alphabet = char, comp = int, compKnot = mfeanswer) {
  int sadd(Subsequence b, int x) {
    return x;
  }

  int cadd(int x, int y) {
    return x + y;
  }

  int nil(void) {
    return 0;
  }

  int is(Subsequence ld, int x, Subsequence rd) {
    Subsequence stem;
    stem.seq = ld.seq;
    stem.i = ld.i;
    stem.j = rd.j;
    
    return x + termau_energy(stem, stem);
  }

  int edl(Subsequence ld, int x, Subsequence rd) {
    Subsequence stem;
    stem.seq = ld.seq;
    stem.i = ld.i+1;
    stem.j = rd.j;
      
    return x + termau_energy(stem, stem) + dl_energy(stem, stem);
  }
 
  int edr(Subsequence ld, int x, Subsequence rd) {
    Subsequence stem;
    stem.seq = ld.seq;
    stem.i = ld.i;
    stem.j = rd.j-1;
      
    return x + termau_energy(stem, stem) + dr_energy(stem, stem);
  }

  int edlr(Subsequence ld, int x, Subsequence rd) {
    Subsequence stem;
    stem.seq = ld.seq;
    stem.i = ld.i+1;
    stem.j = rd.j-1;
      
    return x + termau_energy(stem, stem) + ext_mismatch_energy(stem, stem);
  }

  int pk(mfeanswer x) {
    return x.energy;
  }

  mfeanswer pknot(Subsequence a, int front, Subsequence b, int middle, Subsequence aPrime, int back, Subsequence bPrime ; int stackenergies) {
    mfeanswer res;
	
    Subsequence alphaOuter;
    alphaOuter.seq = a.seq;
    alphaOuter.i = a.i;
    alphaOuter.j = aPrime.j;
    
    Subsequence alphaInner;
    alphaInner.seq = a.seq;
    alphaInner.i = a.j-1;
    alphaInner.j = aPrime.i+1;
    
    Subsequence betaOuter;
    betaOuter.seq = b.seq;
    betaOuter.i = b.i;
    betaOuter.j = bPrime.j;
    
    Subsequence betaInner;
    betaInner.seq = b.seq;
    betaInner.i = b.j-1;
    betaInner.j = bPrime.i+1;
	  
    res.betaLeftOuter = b.i;
    res.alphaRightOuter = aPrime.j;
    
    res.energy =   stackenergies                         // stacking energies
                 + pkinit                                // initiation energy for pk
                 + 3*npp                                 // penalty for 1+2 explicitly unpaired bases
                 + front                                 // energy from front substructure
                 + middle                                // energy from middle substructure
                 + back                                  // energy from back substructure
                 + termau_energy(alphaOuter, alphaOuter) // AU penalty for outmost BP in alpha helix
                 + termau_energy(alphaInner, alphaInner) // AU penalty for innermost BP in alpha helix
                 + termau_energy(betaOuter, betaOuter)   // AU penalty for outmost BP in beta helix
                 + termau_energy(betaInner, betaInner)   // AU penalty for innermost BP in beta helix
                 + dli_energy(alphaInner, alphaInner)    // explicitly unpaired base, before front, dangles at the inside of helix alpha
		         + dri_energy(betaInner, betaInner);     // explicitly unpaired base, after back, dangles at the inside of helix beta
    
	return res;
  }
  mfeanswer pkiss(Subsequence a, int front, Subsequence b, int middle1, Subsequence aPrime, int middle2, Subsequence c, int middle3, Subsequence bPrime, int back, Subsequence cPrime ; int stackenergies) {
    mfeanswer res;
	
	Subsequence alphaOuter;
    alphaOuter.seq = a.seq;
    alphaOuter.i = a.i;
    alphaOuter.j = aPrime.j;
    
    Subsequence alphaInner;
    alphaInner.seq = a.seq;
    alphaInner.i = a.j-1;
    alphaInner.j = aPrime.i+1;
    
    Subsequence betaOuter;
    betaOuter.seq = b.seq;
    betaOuter.i = b.i;
    betaOuter.j = bPrime.j;
    
    Subsequence betaInner;
    betaInner.seq = b.seq;
    betaInner.i = b.j-1;
    betaInner.j = bPrime.i+1;

    Subsequence gammaOuter;
    gammaOuter.seq = c.seq;
    gammaOuter.i = c.i;
    gammaOuter.j = cPrime.j;
    
    Subsequence gammaInner;
    gammaInner.seq = c.seq;
    gammaInner.i = c.j-1;
    gammaInner.j = cPrime.i+1;

    res.betaLeftOuter = c.i;
    res.alphaRightOuter = aPrime.j;

	res.energy =   stackenergies                         // stacking energies
                 + pkissinit                             // initiation energy for pk
                 + 4*npp                                 // penalty for 1+2+1 explicitly unpaired bases
                 + front                                 // energy from front substructure
                 + middle1                               // energy from middle1 substructure
                 + middle2                               // energy from middle2 substructure
                 + middle3                               // energy from middle3 substructure
                 + back                                  // energy from back substructure
                 + termau_energy(alphaOuter, alphaOuter) // AU penalty for outmost BP in alpha helix
                 + termau_energy(alphaInner, alphaInner) // AU penalty for innermost BP in alpha helix
                 + termau_energy(betaOuter, betaOuter)   // AU penalty for outmost BP in beta helix
                 + termau_energy(betaInner, betaInner)   // AU penalty for innermost BP in beta helix
                 + termau_energy(gammaOuter, gammaOuter) // AU penalty for outmost BP in gamma helix
                 + termau_energy(gammaInner, gammaInner) // AU penalty for innermost BP in gamma helix
                 + dli_energy(alphaInner, alphaInner)    // explicitly unpaired base, before front, dangles at the inside of helix alpha
		         + dri_energy(gammaInner, gammaInner)    // explicitly unpaired base, after back, dangles at the inside of helix gamma
		         + dr_energy(alphaOuter, alphaOuter)     // explicitly unpaired base, before middle2, dangles at the outside of helix alpha
		         + dl_energy(gammaOuter, gammaOuter)     // explicitly unpaired base, after middle2, dangles at the outside of helix gamma
    ;
	
	return res;

  }
  int kndl(Subsequence ld, mfeanswer x) {
    Subsequence alpha;
    alpha.seq = ld.seq;
    alpha.i = ld.i+1;
    alpha.j = x.alphaRightOuter;
    
    return x.energy + npp + dl_energy(alpha, alpha);
  }

  int kndr(mfeanswer x, Subsequence rd) {
    Subsequence beta;
    beta.seq = rd.seq;
    beta.i = x.betaLeftOuter;
    beta.j = rd.j-1;
    
    return x.energy + npp + dr_energy(beta, beta);
  }

  int kndlr(Subsequence ld, mfeanswer x, Subsequence rd) {
    Subsequence alpha;
    alpha.seq = ld.seq;
    alpha.i = ld.i+1;
    alpha.j = x.alphaRightOuter;
      
    Subsequence beta;
    beta.seq = ld.seq;
    beta.i = x.betaLeftOuter;
    beta.j = rd.j-1;
      
    return x.energy + 2*npp + dl_energy(alpha, alpha) + dr_energy(beta, beta);
  }

  int sr(Subsequence lb, int x, Subsequence rb) {
    Subsequence stem;
    stem.seq = lb.seq;
    stem.i = lb.i;
    stem.j = rb.j;
      
    return x + sr_energy(stem, stem);
  }

  int hl(Subsequence llb, Subsequence lb, Subsequence r, Subsequence rb, Subsequence rrb) {
    Subsequence outerStem;
    outerStem.seq = llb.seq;
    outerStem.i = llb.i;
    outerStem.j = rrb.j;
      
    Subsequence innerStem;
    innerStem.seq = lb.seq;
    innerStem.i = lb.i;
    innerStem.j = rb.j;
    
    return hl_energy(r) + sr_energy(outerStem, outerStem);
  }

  int bl(Subsequence llb, Subsequence lb, Subsequence lr, int x, Subsequence rb, Subsequence rrb) {
    Subsequence outerStem;
    outerStem.seq = llb.seq;
    outerStem.i = llb.i;
    outerStem.j = rrb.j;
      
    Subsequence innerStem;
    innerStem.seq = lb.seq;
    innerStem.i = lb.i;
    innerStem.j = rb.j;
    
    return x + sr_energy(outerStem, outerStem) + bl_energy(lr, innerStem);
  }

  int br(Subsequence llb, Subsequence lb, int x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    Subsequence outerStem;
    outerStem.seq = llb.seq;
    outerStem.i = llb.i;
    outerStem.j = rrb.j;
      
    Subsequence innerStem;
    innerStem.seq = lb.seq;
    innerStem.i = lb.i;
    innerStem.j = rb.j;
    
    return x + sr_energy(outerStem, outerStem) + br_energy(innerStem, rr);
  }

  int il(Subsequence llb, Subsequence lb, Subsequence lr, int x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    Subsequence outerStem;
    outerStem.seq = llb.seq;
    outerStem.i = llb.i;
    outerStem.j = rrb.j;
      
    Subsequence innerStem;
    innerStem.seq = lb.seq;
    innerStem.i = lb.i;
    innerStem.j = rb.j;
    
    return x + sr_energy(outerStem, outerStem) + il_energy(lr, rr);
  }

  int ml(Subsequence llb, Subsequence lb, int x, Subsequence rb, Subsequence rrb) {
    Subsequence outerStem;
    outerStem.seq = llb.seq;
    outerStem.i = llb.i;
    outerStem.j = rrb.j;

    Subsequence innerStem;
    innerStem.seq = lb.seq;
    innerStem.i = lb.i;
    innerStem.j = rb.j;
      
    return x + mlinit + sr_energy(outerStem, outerStem) + termau_energy(innerStem, innerStem);
  }

  int mldl(Subsequence llb, Subsequence lb, Subsequence ld, int x, Subsequence rb, Subsequence rrb) {
    Subsequence outerStem;
    outerStem.seq = llb.seq;
    outerStem.i = llb.i;
    outerStem.j = rrb.j;

    Subsequence innerStem;
    innerStem.seq = lb.seq;
    innerStem.i = lb.i;
    innerStem.j = rb.j;
      
    return x + mlinit + sr_energy(outerStem, outerStem) + termau_energy(innerStem, innerStem) + dli_energy(innerStem,innerStem);
  }

  int mldr(Subsequence llb, Subsequence lb, int x, Subsequence rd, Subsequence rb, Subsequence rrb) {
    Subsequence outerStem;
    outerStem.seq = llb.seq;
    outerStem.i = llb.i;
    outerStem.j = rrb.j;

    Subsequence innerStem;
    innerStem.seq = lb.seq;
    innerStem.i = lb.i;
    innerStem.j = rb.j;
      
    return x + mlinit + sr_energy(outerStem, outerStem) + termau_energy(innerStem, innerStem) + dri_energy(innerStem,innerStem);
  }

  int mldlr(Subsequence llb, Subsequence lb, Subsequence ld, int x, Subsequence rd, Subsequence rb, Subsequence rrb) {
    Subsequence outerStem;
    outerStem.seq = llb.seq;
    outerStem.i = llb.i;
    outerStem.j = rrb.j;

    Subsequence innerStem;
    innerStem.seq = lb.seq;
    innerStem.i = lb.i;
    innerStem.j = rb.j;
      
    return x + mlinit + sr_energy(outerStem, outerStem) + termau_energy(innerStem, innerStem) + ml_mismatch_energy(innerStem,innerStem);
  }

  int addss(int x, Subsequence r) {
    return x + ss_energy(r);
  }

  int mlstem(int x) {
    return x + ul_energy();
  }

  int pkml(int x) {
    return x + pkmlinit;
  }


  int frd(int x, Subsequence ld; int betaRightOuter) {
    Subsequence beta;
    beta.seq = ld.seq;
    beta.i = ld.i+1;
    beta.j = betaRightOuter;
      
    return x + npp + dl_energy(beta, beta);
  }

  int ul(int x) {
    return x;
  }

  int emptymid(Subsequence m; int betaRightInner, int alphaLeftInner) {
    return sr_pk_energy(m[m.i-1], m[betaRightInner], m[m.i], m[alphaLeftInner-1]);
  }

  int midbase(Subsequence m; int betaRightInner, int alphaLeftInner) {
    return sr_pk_energy(m[m.i-1], m[betaRightInner], m[m.i+1], m[alphaLeftInner-1]) + npp;
  }

  int middlro(Subsequence m; int betaRightInner, int alphaLeftInner) {
    Subsequence beta;
    beta.seq = m.seq;
    beta.i = m.i-1;
    beta.j = betaRightInner+1;
      
    Subsequence alpha;
    alpha.seq = m.seq;
    alpha.i = alphaLeftInner-1;
    alpha.j = m.j+1;
      
    return 2*npp + dri_energy(alpha, alpha) + dli_energy(beta, beta);
  }

  int midregion(int x) {
    return x;
  }

  int middl(Subsequence ld, int x; int betaRightInner) {
    Subsequence beta;
    beta.seq = ld.seq;
    beta.i = ld.i-1;
    beta.j = betaRightInner+1;

    return x + npp + dli_energy(beta, beta);
  }

  int middr(int x, Subsequence rd; int alphaLeftInner) {
    Subsequence alpha;
    alpha.seq = rd.seq;
    alpha.i = alphaLeftInner-1;
    alpha.j = rd.j+1;

    return x + npp + dri_energy(alpha, alpha);
  }

  int middlr(Subsequence ld, int x, Subsequence rd; int betaRightInner, int alphaLeftInner) {
    Subsequence beta;
    beta.seq = ld.seq;
    beta.i = ld.i-1;
    beta.j = betaRightInner+1;
      
    Subsequence alpha;
    alpha.seq = rd.seq;
    alpha.i = alphaLeftInner-1;
    alpha.j = rd.j+1;

    return x + 2*npp + dli_energy(beta, beta) + dri_energy(alpha, alpha);
  }

  int bkd(Subsequence rd, int x; int alphaLeftOuter) {
    Subsequence alpha;
    alpha.seq = rd.seq;
    alpha.i = alphaLeftOuter;
    alpha.j = rd.j-1;
    
    return x + npp + dr_energy(alpha, alpha);
  }
 
  int pss(Subsequence r) {
    return npp * size(r);
  }

  choice [int] h([int] i) {
    return list(minimum(i));
  }

  choice [mfeanswer] hKnot([mfeanswer] i) {
    return list(minimum(i));
  }
}


algebra pretty implements Algebra(alphabet = char, comp = string_t, compKnot = string_t) {
  string_t sadd(Subsequence b, string_t x) {
    string_t res;
    append(res, '.');
    append(res, x);
    return res;
  }

  string_t cadd(string_t x, string_t y) {
    string_t res;
    append(res, x);
    append(res, y);
    return res;
  }

  string_t nil(void) {
    string_t res;
    return res;
  }

  string_t is(Subsequence ld, string_t x, Subsequence rd) {
    return x;
  }

  string_t edl(Subsequence ld, string_t x, Subsequence rd) {
    string_t res;
    append(res, '.');
    append(res, x);
    return res;
  }
 
  string_t edr(Subsequence ld, string_t x, Subsequence rd) {
    string_t res;
    append(res, x);
    append(res, '.');
    return res;
  }

  string_t edlr(Subsequence ld, string_t x, Subsequence rd) {
    string_t res;
    append(res, '.');
    append(res, x);
    append(res, '.');
    return res;
  }

  string_t pk(string_t x) {
    return x;
  }

  string_t pknot(Subsequence a, string_t frt, Subsequence b, string_t mid, Subsequence at, string_t bck, Subsequence bt ; int stackenergies) {
    string_t res;
    
	append(res, '[', size(a));
    append(res, '.');
    append(res, frt);
    append(res, '{', size(b));
    append(res, mid);
    append(res, ']', size(at));
    append(res, bck);
    append(res, '.', 2);
    append(res, '}', size(bt));
	  
    return res;
  }

  string_t pkiss(Subsequence a, string_t front, Subsequence b, string_t middle1, Subsequence aPrime, string_t middle2, Subsequence c, string_t middle3, Subsequence bPrime, string_t back, Subsequence cPrime ; int stackenergies) {
    string_t res;
    
	append(res, '[', size(a));
    append(res, '.');
    append(res, front);
    append(res, '{', size(b));
    append(res, middle1);
    append(res, ']', size(aPrime));
	append(res, '.');
	append(res, middle2);
	append(res, '.');
	append(res, '<', size(c));
    append(res, middle3);
	append(res, '}', size(bPrime));
    append(res, back);
    append(res, '.');
    append(res, '>', size(cPrime));
	  
    return res;
  }
  
  string_t kndl(Subsequence ld, string_t x) {
    string_t res;
    append(res, '.');
    append(res, x);
    return res;
  }

  string_t kndr(string_t x, Subsequence rd) {
    string_t res;
    append(res, x);
    append(res, '.');
    return res;
  }

  string_t kndlr(Subsequence ld, string_t x, Subsequence rd) {
    string_t res;
    append(res, '.');
    append(res, x);
    append(res, '.');
    return res;
  }

  string_t sr(Subsequence lb, string_t x, Subsequence rb) {
    string_t res;
    append(res, '(');
    append(res, x);
    append(res, ')');
    return res;
  }

  string_t hl(Subsequence llb, Subsequence lb, Subsequence r, Subsequence rb, Subsequence rrb) {
    string_t res;
    append(res, "((", 2);
    append(res, '.', size(r));
    append(res, "))", 2);
    return res;
  }

  string_t bl(Subsequence llb, Subsequence lb, Subsequence lr, string_t x, Subsequence rb, Subsequence rrb) {
    string_t res;
    append(res, "((", 2);
    append(res, '.', size(lr));
    append(res, x);
    append(res, "))", 2);
    return res;
  }

  string_t br(Subsequence llb, Subsequence lb, string_t x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    string_t res;
    append(res, "((", 2);
    append(res, x);
    append(res, '.', size(rr));
    append(res, "))", 2);
    return res;
  }

  string_t il(Subsequence llb, Subsequence lb, Subsequence lr, string_t x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    string_t res;
    append(res, "((", 2);
    append(res, '.', size(lr));
    append(res, x);
    append(res, '.', size(rr));
    append(res, "))", 2);
    return res;
  }

  string_t ml(Subsequence llb, Subsequence lb, string_t x, Subsequence rb, Subsequence rrb) {
    string_t res;
    append(res, "((", 2);
    append(res, x);
    append(res, "))", 2);
    return res;
  }

  string_t mldl(Subsequence llb, Subsequence lb, Subsequence ld, string_t x, Subsequence rb, Subsequence rrb) {
    string_t res;
    append(res, "((", 2);
    append(res, '.');
    append(res, x);
    append(res, "))", 2);
    return res;
  }

  string_t mldr(Subsequence llb, Subsequence lb, string_t x, Subsequence rd, Subsequence rb, Subsequence rrb) {
    string_t res;
    append(res, "((", 2);
    append(res, x);
    append(res, '.');
    append(res, "))", 2);
    return res;
  }

  string_t mldlr(Subsequence llb, Subsequence lb, Subsequence ld, string_t x, Subsequence rd, Subsequence rb, Subsequence rrb) {
    string_t res;
    append(res, "((", 2);
    append(res, '.');
    append(res, x);
    append(res, '.');
    append(res, "))", 2);
    return res;
  }

  string_t addss(string_t x, Subsequence r) {
    string_t res;
    append(res, x);
    append(res, '.', size(r));
    return res;
  }

  string_t mlstem(string_t x) {
    return x;
  }

  string_t pkml(string_t x) {
    return x;
  }

  string_t frd(string_t x, Subsequence ld; int betaRightOuter) {
    string_t res;
    append(res, x);
    append(res, '.');
    return res;
  }

  string_t ul(string_t x) {
    return x;
  }

  string_t emptymid(Subsequence m; int betaRightInner, int alphaLeftInner) {
    string_t res;
    return res;
  }

  string_t midbase(Subsequence m; int betaRightInner, int alphaLeftInner) {
    string_t res;
    append(res, '.');
    return res;
  }

  string_t middlro(Subsequence m; int betaRightInner, int alphaLeftInner) {
    string_t res;
    append(res, "..", 2);
    return res;
  }

  string_t midregion(string_t x) {
    return x;
  }

  string_t middl(Subsequence ld, string_t x;  int betaRightInner) {
    string_t res;
    append(res, '.');
    append(res, x);
    return res;
  }

  string_t middr(string_t x, Subsequence rd;  int alphaLeftInner) {
    string_t res;
    append(res, x);
    append(res, '.');
    return res;
  }

  string_t middlr(Subsequence ld, string_t x, Subsequence rd; int betaRightInner, int alphaLeftInner) {
    string_t res;
    append(res, '.');
    append(res, x);
    append(res, '.');
    return res;
  }

  string_t bkd(Subsequence rd, string_t x; int alphaLeftOuter) {
    string_t res;
    append(res, '.');
    append(res, x);
    return res;
  }
 
  string_t pss(Subsequence r) {
    string_t res;
    append(res, '.', size(r));
    return res;
  }

  choice [string_t] h([string_t] i) {
    return unique(i);
  }

  choice [string_t] hKnot([string_t] i) {
    return unique(i);
  }
}

algebra enforce implements Algebra(alphabet = char, comp = myBool, compKnot = myBool) {
  myBool sadd(Subsequence b, myBool x) {
    return x;
  }

  myBool cadd(myBool x, myBool y) {
	if ((x == 1) || (y == 1)) {
		return 1;
	} else {
		return 0;
	}
  }

  myBool nil(void) {
    return 0;
  }

  myBool is(Subsequence ld, myBool x, Subsequence rd) {
    return x;
  }

  myBool edl(Subsequence ld, myBool x, Subsequence rd) {
    return x;
  }
 
  myBool edr(Subsequence ld, myBool x, Subsequence rd) {
    return x;
  }

  myBool edlr(Subsequence ld, myBool x, Subsequence rd) {
    return x;
  }

  myBool pk(myBool x) {
    return x;
  }

  myBool pknot(Subsequence a, myBool frt, Subsequence b, myBool mid, Subsequence at, myBool bck, Subsequence bt ; int stackenergies) {
    return 1;
  }

  myBool pkiss(Subsequence a, myBool front, Subsequence b, myBool middle1, Subsequence aPrime, myBool middle2, Subsequence c, myBool middle3, Subsequence bPrime, myBool back, Subsequence cPrime ; int stackenergies) {
    return 1;
  }
  
  myBool kndl(Subsequence ld, myBool x) {
    return x;
  }

  myBool kndr(myBool x, Subsequence rd) {
    return x;
  }

  myBool kndlr(Subsequence ld, myBool x, Subsequence rd) {
    return x;
  }

  myBool sr(Subsequence lb, myBool x, Subsequence rb) {
    return x;
  }

  myBool hl(Subsequence llb, Subsequence lb, Subsequence r, Subsequence rb, Subsequence rrb) {
    return 0;
  }

  myBool bl(Subsequence llb, Subsequence lb, Subsequence lr, myBool x, Subsequence rb, Subsequence rrb) {
    return x;
  }

  myBool br(Subsequence llb, Subsequence lb, myBool x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    return x;
  }

  myBool il(Subsequence llb, Subsequence lb, Subsequence lr, myBool x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    return x;
  }

  myBool ml(Subsequence llb, Subsequence lb, myBool x, Subsequence rb, Subsequence rrb) {
    return x;
  }

  myBool mldl(Subsequence llb, Subsequence lb, Subsequence ld, myBool x, Subsequence rb, Subsequence rrb) {
    return x;
  }

  myBool mldr(Subsequence llb, Subsequence lb, myBool x, Subsequence rd, Subsequence rb, Subsequence rrb) {
    return x;
  }

  myBool mldlr(Subsequence llb, Subsequence lb, Subsequence ld, myBool x, Subsequence rd, Subsequence rb, Subsequence rrb) {
    return x;
  }

  myBool addss(myBool x, Subsequence r) {
    return x;
  }

  myBool mlstem(myBool x) {
    return x;
  }

  myBool pkml(myBool x) {
    return x;
  }

  myBool frd(myBool x, Subsequence ld; int betaRightOuter) {
    return x;
  }

  myBool ul(myBool x) {
    return x;
  }

  myBool emptymid(Subsequence m; int betaRightInner, int alphaLeftInner) {
    return 0;
  }

  myBool midbase(Subsequence m; int betaRightInner, int alphaLeftInner) {
    return 0;
  }

  myBool middlro(Subsequence m; int betaRightInner, int alphaLeftInner) {
    return 0;
  }

  myBool midregion(myBool x) {
    return x;
  }

  myBool middl(Subsequence ld, myBool x;  int betaRightInner) {
    return x;
  }

  myBool middr(myBool x, Subsequence rd;  int alphaLeftInner) {
    return x;
  }

  myBool middlr(Subsequence ld, myBool x, Subsequence rd; int betaRightInner, int alphaLeftInner) {
    return x;
  }

  myBool bkd(Subsequence rd, myBool x; int alphaLeftOuter) {
    return x;
  }
 
  myBool pss(Subsequence r) {
    return 0;
  }

  choice [myBool] h([myBool] i) {
    return unique(i);
  }

  choice [myBool] hKnot([myBool] i) {
    return unique(i);
  }
}


algebra shape5 implements Algebra(alphabet = char, comp = myShape, compKnot = myShape) {
  myShape sadd(Subsequence b, myShape x) {
    return x;
  }

  myShape cadd(myShape x, myShape y) {
    myShape res;
    append(res, x);
    append(res, y);
    return res;
  }

  myShape nil(void) {
    myShape res;
    return res;
  }

  myShape is(Subsequence ld, myShape x, Subsequence rd) {
    return x;
  }

  myShape edl(Subsequence ld, myShape x, Subsequence rd) {
    return x;
  }
 
  myShape edr(Subsequence ld, myShape x, Subsequence rd) {
    return x;
  }

  myShape edlr(Subsequence ld, myShape x, Subsequence rd) {
    return x;
  }

  myShape pk(myShape x) {
    return x;
  }

  myShape pknot(Subsequence a, myShape frt, Subsequence b, myShape mid, Subsequence at, myShape bck, Subsequence bt; int stackenergies) {
    myShape res;
	  
    append(res, '[');
    append(res, frt);
    append(res, '{');
    append(res, mid);
    append(res, ']');
    append(res, bck);
    append(res, '}');
	  
    return res;
  }

  myShape pkiss(Subsequence a, myShape frt, Subsequence b, myShape middle1, Subsequence aPrime, myShape middle2, Subsequence c, myShape middle3, Subsequence bPrime, myShape bck, Subsequence cPrime ; int stackenergies) {
    myShape res;
	  
    append(res, '[');
    append(res, frt);
    append(res, '{');
    append(res, middle1);
    append(res, ']');
    append(res, middle2);
    append(res, '<');
    append(res, middle3);
    append(res, '}');
    append(res, bck);
    append(res, '>');
	  
    return res;
  }
  
  myShape kndl(Subsequence ld, myShape x) {
    return x;
  }

  myShape kndr(myShape x, Subsequence rd) {
    return x;
  }

  myShape kndlr(Subsequence ld, myShape x, Subsequence rd) {
    return x;
  }

  myShape sr(Subsequence lb, myShape x, Subsequence rb) {
    return x;
  }

  myShape hl(Subsequence llb, Subsequence lb, Subsequence r, Subsequence rb, Subsequence rrb) {
    myShape res;
    append(res, "[]", 2);
    return res;
  }

  myShape bl(Subsequence llb, Subsequence lb, Subsequence lr, myShape x, Subsequence rb, Subsequence rrb) {
    return x;
  }

  myShape br(Subsequence llb, Subsequence lb, myShape x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    return x;
  }

  myShape il(Subsequence llb, Subsequence lb, Subsequence lr, myShape x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    return x;
  }

  myShape ml(Subsequence llb, Subsequence lb, myShape x, Subsequence rb, Subsequence rrb) {
    myShape res;
    append(res, '[');
    append(res, x);
    append(res, ']');
    return res;
  }

  myShape mldl(Subsequence llb, Subsequence lb, Subsequence ld, myShape x, Subsequence rb, Subsequence rrb) {
    myShape res;
    append(res, '[');
    append(res, x);
    append(res, ']');
    return res;
  }

  myShape mldr(Subsequence llb, Subsequence lb, myShape x, Subsequence rd, Subsequence rb, Subsequence rrb) {
    myShape res;
    append(res, '[');
    append(res, x);
    append(res, ']');
    return res;
  }

  myShape mldlr(Subsequence llb, Subsequence lb, Subsequence ld, myShape x, Subsequence rd, Subsequence rb, Subsequence rrb) {
    myShape res;
    append(res, '[');
    append(res, x);
    append(res, ']');
    return res;
  }

  myShape addss(myShape x, Subsequence r) {
    return x;
  }

  myShape mlstem(myShape x) {
    return x;
  }

  myShape pkml(myShape x) {
    return x;
  }

  myShape frd(myShape x, Subsequence ld; int betaRightOuter) {
    return x;
  }

  myShape ul(myShape x) {
    return x;
  }

  myShape emptymid(Subsequence m; int betaRightInner, int alphaLeftInner) {
    myShape res;
    return res;
  }

  myShape midbase(Subsequence m; int betaRightInner, int alphaLeftInner) {
    myShape res;
    return res;
  }

  myShape middlro(Subsequence m; int betaRightInner, int alphaLeftInner) {
    myShape res;
    return res;
  }

  myShape midregion(myShape x) {
    return x;
  }

  myShape middl(Subsequence ld, myShape x;  int betaRightInner) {
    return x;
  }

  myShape middr(myShape x, Subsequence rd;  int alphaLeftInner) {
    return x;
  }

  myShape middlr(Subsequence ld, myShape x, Subsequence rd; int betaRightInner, int alphaLeftInner) {
    return x;
  }

  myShape bkd(Subsequence rd, myShape x; int alphaLeftOuter) {
    return x;
  }
 
  myShape pss(Subsequence r) {
    myShape res;
    return res;
  }

  choice [myShape] h([myShape] i) {
    return unique(i);
  }

  choice [myShape] hKnot([myShape] i) {
    return unique(i);
  }
}

algebra shape4 extends shape5 {
  myShape il(Subsequence llb, Subsequence lb, Subsequence lr, myShape x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    myShape res;
	append(res, '[');
	append(res, x);
	append(res, ']');
	return res;
  }
}

algebra shape3 extends shape5 {
  myShape bl(Subsequence llb, Subsequence lb, Subsequence lr, myShape x, Subsequence rb, Subsequence rrb) {
    myShape res;
	append(res, '[');
	append(res, x);
	append(res, ']');
	return res;
  }
  myShape br(Subsequence llb, Subsequence lb, myShape x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    myShape res;
	append(res, '[');
	append(res, x);
	append(res, ']');
	return res;
  }
  myShape il(Subsequence llb, Subsequence lb, Subsequence lr, myShape x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    myShape res;
	append(res, '[');
	append(res, x);
	append(res, ']');
	return res;
  }
}

algebra shape2 extends shape5 {
  myShape bl(Subsequence llb, Subsequence lb, Subsequence lr, myShape x, Subsequence rb, Subsequence rrb) {
    myShape res;
	append(res, '[');
	append(res, '_');
	append(res, x);
	append(res, ']');
	return res;
  }
  myShape br(Subsequence llb, Subsequence lb, myShape x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    myShape res;
	append(res, '[');
	append(res, x);
	append(res, '_');
	append(res, ']');
	return res;
  }
  myShape il(Subsequence llb, Subsequence lb, Subsequence lr, myShape x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    myShape res;
	append(res, '[');
	append(res, '_');
	append(res, x);
	append(res, '_');
	append(res, ']');
	return res;
  }
}

algebra shape1 extends shape5 {
  myShape sadd(Subsequence b, myShape x) {
    if (front(x) == '_') {
      return x;
    } else {
      myShape res;
      append(res, '_');
      append(res, x);
      return res;
    }
  }
  myShape cadd(myShape x, myShape y) {
    myShape res;
    if (back(x) == '_' && front(y) == '_') {
      append(res, x);
      append(res, tail(y));
    } else {
      append(res, x);
      append(res, y);
    }
    return res;
  }
  myShape edl(Subsequence ld, myShape x, Subsequence rd) {
    myShape res;
	append(res, '_');
    append(res, x);
    return res;
  }
  myShape edr(Subsequence ld, myShape x, Subsequence rd) {
    myShape res;
    append(res, x);
    append(res, '_');
    return res;
  }
  myShape edlr(Subsequence ld, myShape x, Subsequence rd) {
    myShape res;
    append(res, '_');
    append(res, x);
    append(res, '_');
    return res;
  }
  myShape pknot(Subsequence a, myShape frt, Subsequence b, myShape mid, Subsequence at, myShape bck, Subsequence bt ; int stackenergies) {
    myShape res;
    
    if (front(frt) == '_') {
      append(res, '[');
    } else {
      append(res, '[');
      append(res, '_');
    }
	append(res, frt);
    append(res, '{');
    append(res, mid);
    append(res, ']');
    if (back(bck) == '_') {
      append(res, bck);
    } else {
      append(res, bck);
      append(res, '_');
    }
    append(res, '}');
    
    return res;
  }
  myShape pkiss(Subsequence a, myShape front, Subsequence b, myShape middle1, Subsequence aPrime, myShape middle2, Subsequence c, myShape middle3, Subsequence bPrime, myShape back, Subsequence cPrime ; int stackenergies) {
    return front;
  }

  myShape kndl(Subsequence ld, myShape x) {
    myShape res;
    append(res, '_');
    append(res, x);
    return res;
  }
  myShape kndr(myShape x, Subsequence rd) {
    myShape res;
    append(res, x);
    append(res, '_');
    return res;
  }
  myShape kndlr(Subsequence ld, myShape x, Subsequence rd) {
    myShape res;
    append(res, '_');
    append(res, x);
    append(res, '_');
    return res;
  }
  myShape bl(Subsequence llb, Subsequence lb, Subsequence lr, myShape x, Subsequence rb, Subsequence rrb) {
    myShape res;
    append(res, '[');
    append(res, '_');
    append(res, x);
    append(res, ']');
    return res;
  }
  myShape br(Subsequence llb, Subsequence lb, myShape x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    myShape res;
    append(res, '[');
    append(res, x);
    append(res, '_');
    append(res, ']');
    return res;
  }
  myShape il(Subsequence llb, Subsequence lb, Subsequence lr, myShape x, Subsequence rr, Subsequence rb, Subsequence rrb) {
    myShape res;
    append(res, '[');
    append(res, '_');
    append(res, x);
    append(res, '_');
    append(res, ']');
    return res;
  }
  myShape mldl(Subsequence llb, Subsequence lb, Subsequence ld, myShape x, Subsequence rb, Subsequence rrb) {
    myShape res;
    append(res, '[');
    if (front(x) == '_') {
      append(res, '[');
    } else {
      append(res, '[');
      append(res, '_');
    }
    append(res, x);
    append(res, ']');
    return res;
  }
  myShape mldr(Subsequence llb, Subsequence lb, myShape x, Subsequence rd, Subsequence rb, Subsequence rrb) {
    myShape res;
    append(res, '[');
    if (back(x) == '_') {
      append(res, x);
    } else {
      append(res, x);
      append(res, '_');
    }
    append(res, ']');
    return res;
  }
  myShape mldlr(Subsequence llb, Subsequence lb, Subsequence ld, myShape x, Subsequence rd, Subsequence rb, Subsequence rrb) {
    myShape res;
    if (front(x) == '_') {
      append(res, '[');
    } else {
      append(res, '[');
      append(res, '_');
    }
    if (back(x) == '_') {
      append(res, x);
    } else {
      append(res, x);
      append(res, '_');
    }
    append(res, ']');
    return res;
  }

  myShape addss(myShape x, Subsequence r) {
    myShape res;
    if (back(x) == '_') {
      append(res, x);
    } else {
      append(res, x);
      append(res, '_');
    }
    return res;
  }
  myShape frd(myShape x, Subsequence ld; int betaRightOuter) {
    myShape res;
    if (back(x) == '_') {
      append(res, x);
    } else {
      append(res, x);
      append(res, '_');
    }
    return res;
  }
  myShape midbase(Subsequence m; int betaRightInner, int alphaLeftInner) {
    myShape res;
    append(res, '_');
    return res;
  }

  myShape middlro(Subsequence m; int betaRightInner, int alphaLeftInner) {
    myShape res;
    append(res, '_');
  return res;
  }

  myShape midregion(myShape x) {
    return x;
  }

  myShape middl(Subsequence ld, myShape x;  int betaRightInner) {
    myShape res;
    append(res, '_');
    append(res, x);
    return res;
  }

  myShape middr(myShape x, Subsequence rd;  int alphaLeftInner) {
    myShape res;
    append(res, x);
    append(res, '_');
    return res;
  }

  myShape middlr(Subsequence ld, myShape x, Subsequence rd; int betaRightInner, int alphaLeftInner) {
    myShape res;
    append(res, '_');
    append(res, x);
    append(res, '_');
    return res;
  }

  myShape bkd(Subsequence rd, myShape x; int alphaLeftOuter) {
    if (front(x) == '_') {
      return x;
    } else {
      myShape res;
	  append(res, '_');
	  append(res, x);
	  return res;
    }
  }
  myShape pss(Subsequence r) {
    myShape res;
    if (size(r) > 0) {
      append(res, '_');
    }
    return res;
  }
}



grammar pknotsRG uses Algebra(axiom = struct) {
	
    struct       = sadd(BASE,      struct) |
                   cadd(dangle_Pr, struct) |
                   nil (EMPTY) 
                   # h;

    dangle_Pr    = dangle | 
                   dangleknot
                   # h;
    
    dangle       = is   (LOC,  closed, LOC ) |
                   edl  (BASE, closed, LOC ) |
                   edr  (LOC,  closed, BASE) |
                   edlr (BASE, closed, BASE) 
                   # h;
    
    dangleknot   = pk   (      knot        ) |
                   kndl (BASE, knot        ) |
                   kndr (      knot,   BASE) |
                   kndlr(BASE, knot,   BASE) 
                   # h;

    closed       ={stack   | 
                   hairpin |
                   leftB   | 
                   rightB  | 
                   iloop   | 
                   multiloop} with stackpairing 
                   # h;

    stack        = sr   (      BASE,                          closed,                                            BASE      ) # h;
    hairpin      = hl   (BASE, BASE,                          {REGION with minsize(3)},                          BASE, BASE) # h;
    leftB        = bl   (BASE, BASE, REGION with maxsize(30), closed,                                            BASE, BASE) # h;
    rightB       = br   (BASE, BASE,                          closed,                   REGION with maxsize(30), BASE, BASE) # h;
    iloop        = il   (BASE, BASE, REGION with maxsize(30), closed,                   REGION with maxsize(30), BASE, BASE) # h;
    
    multiloop    ={ml   (BASE, BASE,                          ml_comps1,                                         BASE, BASE) |
                   mldl (BASE, BASE, BASE,                    ml_comps1,                                         BASE, BASE) |
                   mldr (BASE, BASE,                          ml_comps1,                BASE,                    BASE, BASE) |
                   mldlr(BASE, BASE, BASE,                    ml_comps1,                BASE,                    BASE, BASE) } with stackpairing
                   # h;

    ml_comps1    = sadd (BASE,             ml_comps1) |
                   cadd (mldangle,         ml_comps)  |
                   addss(pkml(dangleknot), REGION0)
                   # h ;

                     
    ml_comps     = sadd (BASE,             ml_comps) |
                   cadd (mldangle,         ml_comps) |
                   addss(mldangle,         REGION0)
                   # h ;

    mldangle     = mlstem(dangle)     |
                   pkml  (dangleknot)
                   # h;
    
    knot         = help_pknot
                   //| help_pkiss
                     # hKnot;
				   
    help_pknot   = 
      .[
         int i = t_0_i;
         int j = t_0_j;
         if (!(j - i < 11))
           for (int l = (i + 7); (l <= (j - 4)); l=l+1) {
             int alphamaxlen = second(stacklen(t_0_seq, i, l));
             if (alphamaxlen < 2)
               continue;
             for (int k = i+3; k <= l-4; k=k+1) {
               int alphareallen = min(alphamaxlen, k-i-1);
               if (alphareallen < 2)
                 continue;
         
               int betamaxlen = second(stacklen(t_0_seq, k, j));
               if (betamaxlen < 2)
                 continue;

               int betatemplen = min(betamaxlen, j-l-2);
               if (betatemplen<2)
                 continue;

               int betareallen = min(betatemplen, l-k-alphareallen);
               if (betareallen < 2)
                 continue;

               int stackenergies = first(stacklen(t_0_seq, i, l))  // maximal alpha helix
                   + first(stacklen(t_0_seq, k, j)) // maximal beta helix
                   // reduced part of alpha helix
                   - first(stacklen(t_0_seq, i+alphareallen-1, l-alphareallen+1))
                   // reduced part of beta helix
                   - first(stacklen(t_0_seq, k+betareallen-1, j-betareallen+1));

                INNER(CODE);
              }
            }
       ].
      {
         pknot(REGION, REGION, REGION) .{
           pknot(REGION[i, i+alphareallen],
              front[i+alphareallen+1, k] .(j).,
              REGION[k, k+betareallen],
              middle[k+betareallen, l-alphareallen] .(j-betareallen, i+alphareallen).,
              REGION[l-alphareallen, l],
              back[l, j-betareallen-2] .(i).,
              REGION[j-betareallen, j] ;
              stackenergies) 
         }.
      } # hKnot;    
    
    help_pkiss   = 
      .[
         int i = t_0_i;
         int j = t_0_j;
         int h = t_0_k_0;
         int k = t_0_k_1;
         int l = t_0_k_2;
         int m = t_0_k_3;
		 //~ if (j-i<16)
           //~ continue;
		 if (i+3>h || h+4>k || k+2>l || l+4>m || m+3>j)
			 continue;
		 
         int alphamaxlen = second(stacklen(t_0_seq, i, k));
         if (alphamaxlen < 2)
           continue;
         int alphareallen = min(alphamaxlen, h-i-1);
         if (alphareallen < 2)
           continue;
		 
         int gammamaxlen = second(stacklen(t_0_seq, l, j));
         if (gammamaxlen < 2)
           continue;
         int gammareallen = min(gammamaxlen, j-m-1);
         if (gammareallen < 2)
           continue;

         int betamaxlen = second(stacklen(t_0_seq, h, m));
		 int betareallen = min(min(betamaxlen, k-h-alphareallen), min(betamaxlen, m-l-gammareallen));
		 if (betareallen < 2)
           continue;
         
		 int stackenergies =      first(stacklen(t_0_seq, i, k)) 								// maximal alpha helix
								+ first(stacklen(t_0_seq, h, m)) 								// maximal beta helix
								+ first(stacklen(t_0_seq, l, j)) 								// maximal gamma helix
		                        - first(stacklen(t_0_seq, i+alphareallen-1, k-alphareallen+1))  // reduced part of alpha helix
		                        - first(stacklen(t_0_seq, h+betareallen-1, m-betareallen+1))    // reduced part of beta helix
		                        - first(stacklen(t_0_seq, l+gammareallen-1, j-gammareallen+1)); // reduced part of gamma helix
       ].
      {
         pkiss(REGION, REGION, REGION, REGION, REGION) .{
           pkiss(REGION[i, i+alphareallen],													           //alpha open
                 front[i+alphareallen+1, h] .(m).,											           //front
                 REGION[h, h+betareallen],													           //beta open
                 middle[h+betareallen, k-alphareallen] .(m-betareallen, i+alphareallen).,	           //middle 1
                 REGION[k-alphareallen, k],													           //alpha close
                 middleNoDangling[k+1, l-1],	                                                       //middle 2
                 REGION[l, l+gammareallen],													           //gamma open
                 middleNoCoaxStack[l+gammareallen, m-betareallen] .(j-gammareallen, h+betareallen).,   //middle 3
                 REGION[m-betareallen, m],													           //beta close
                 back[m, j-gammareallen-1] .(h).,											           //back
                 REGION[j-gammareallen, j] ;													       //gamma close
                 stackenergies) 
         }.
      } # hKnot;    
    	  
    front(int betaRightOuter) = front_Pr               |
                                frd  (front_Pr, BASE; betaRightOuter)
                                # h;
              
    front_Pr     = ul(emptystrand) |
                   pk_comps
                   # h;
               
    middle(int betaRightInner, int alphaLeftInner)            = emptymid  (REGION0        ; betaRightInner, alphaLeftInner) with minsize(0) with maxsize(0) |
                                                                midbase   (REGION0        ; betaRightInner, alphaLeftInner) with minsize(1) with maxsize(1) |
                                                                middlro   (REGION0        ; betaRightInner, alphaLeftInner) with minsize(2) with maxsize(2) |
                                                                midregion (      mid                                      )                                 |
                                                                middl     (BASE, mid      ; betaRightInner                )                                 |
                                                                middr     (      mid, BASE;                 alphaLeftInner)                                 |
                                                                middlr    (BASE, mid, BASE; betaRightInner, alphaLeftInner) 
                                                                # h;

    middleNoDangling                                          = mid                                                                                         |
																nil(EMPTY) 
	                                                            # h;

    middleNoCoaxStack(int betaRightInner, int alphaLeftInner) = nil       (EMPTY)                                                                           |
                                                                middlro   (REGION0        ; betaRightInner, alphaLeftInner) with minsize(2) with maxsize(2) |
                                                                midregion (      mid                                      )                                 |
                                                                middl     (BASE, mid      ; betaRightInner                )                                 |
                                                                middr     (      mid, BASE;                 alphaLeftInner)                                 |
                                                                middlr    (BASE, mid, BASE; betaRightInner, alphaLeftInner) 
                                                                # h;
	  
    mid          = ul(singlestrand) |
                   pk_comps
                   # h;
          
    back(int alphaLeftOuter) = back_Pr               |
                               bkd(BASE, back_Pr; alphaLeftOuter) 
                               # h;
             
    back_Pr      = ul(emptystrand) |
                   pk_comps
                   # h;
              
    pk_comps     = cadd(singlestrand, pk_comps)    |
                   cadd(mldangle, pk_comps)        |
                   cadd(mldangle, ul(emptystrand)) 
                   # h;
    
    singlestrand = pss(REGION) # h;
    
    emptystrand  = pss(REGION0) # h ;

}


instance pretty = pknotsRG(pretty) ;
instance mfe = pknotsRG(mfe) ;
instance mfepp = pknotsRG(mfe * pretty);
instance mfeppenf = pknotsRG((mfe * pretty) * enforce);
instance ppmfe = pknotsRG(pretty * mfe);
instance ppmfeenf = pknotsRG((pretty * mfe) * enforce);
instance shape5mfepp = pknotsRG((shape5 * mfe) * pretty);
instance enfmfepp = pknotsRG((enforce * mfe) * pretty);

/* Beispiel, warum stacklen nicht nur durch # moeglicher BP berechnet werden kann, denn GU auf UG gibt destabilisierende Energie!	
acgucgaaauaaaugccuugucugcuauauucgacgcgagcuuaauauuuggggcc
.[[[[[[[......{{{{{..........]]]]]]]..............}}}}}. 
.[[[[[[[......{{{{{{.........]]]]]]].............}}}}}}.
*/

/* Beispiel fuer nicht funktionierende Shape Algebra
acgucgaaauaaaugccuugucugcuauauucgacg
( ( [{]}[] , (430, 5, 15) ) , .[[[.{{.....]]]..}}(((..........))). )
( ( [{]}[] , (430, 5, 15) ) , .[[[..{{....]]]..}}(((..........))). )

// Shape Datentyp wurde auf { } < > erweitert
// TODO Shape Algebren fertig bekommen

/* CGGCACCCAGCCGGGGCGGAGUCCGCGAAUGGG */

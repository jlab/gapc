import rna
import "singlefold.hh"

input rna

type Rope = extern
type string_t = Rope

signature Algebra(alphabet, comp, compKnot) {
  comp sadd(Subsequence, comp);
  comp ssadd(Subsequence, comp);
  comp cadd(comp, comp);
  comp nil(void);
  comp is(Subsequence, comp, Subsequence);
  comp edl(Subsequence, comp, Subsequence);
  comp edr(Subsequence, comp, Subsequence);
  comp edlr(Subsequence, comp, Subsequence);
  comp pk(compKnot);
  compKnot pknot(Subsequence, comp, Subsequence, comp, Subsequence, comp, Subsequence ; int);
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





algebra enum auto enum;
algebra pretty implements Algebra(alphabet = char, comp = string_t, compKnot = string_t) {
  string_t sadd(Subsequence b, string_t x) {
    string_t res;
    append(res, '.');
    append(res, x);
    return res;
  }
  string_t ssadd(Subsequence e, string_t x) {
    string_t res;
    append(res, '.', size(e));
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


grammar fold uses Algebra (axiom = structstart) {
 
 	 structstart = sadd( BASE , structstart ) | addss( rnastruct  , UREGION )  # h;

	 rnastruct = motif0 # h;

	 motif0 =  is( LOC , stem0 , LOC )   # h;
	//ys fix:// stem0 = sr( BASE   , sr( BASE  , sr( BASE   , sr( BASE   , sr( BASE  , sr( BASE  , sr( BASE  , sr( BASE  , motif1 , BASE ) with basepair , BASE ) with basepair , BASE ) with basepair , BASE ) with basepair , BASE  ) with basepair , BASE  ) with basepair , BASE  ) with basepair , BASE  ) with basepair # h ;
	stem0 = sr(BASE, motif1, BASE) with basepair # h;
	
	 motif1 = hairpinloop0 # h ;
 	 hairpinloop0 = hl( BASE , BASE , REGION with minsize(3)  , BASE , BASE ) with basepair # h ;

}
instance pretty = fold ( pretty );


import rna
//import pf_filter

input rna

type shape_t = shape
type base_t = extern
type Rope = extern

signature stefansAlgebra(alphabet, answer) {
  answer root(answer);
  answer unpaired(Subsequence);
  answer lasthlnoss(answer);
  answer lasthlss(answer, Subsequence);
  answer nexthl(answer, answer);
  answer lastmlnoss(answer, answer);
  answer lastmlss(answer, answer, Subsequence);
  answer nextml(answer, answer);
  answer addRegion(Subsequence, answer);
  answer startstem(answer);
  answer drem(Subsequence, answer, Subsequence);
  answer edlr(Subsequence, answer, Subsequence);
  answer edl(Subsequence, answer, Subsequence);
  answer edr(Subsequence, answer, Subsequence);
  answer stack(Subsequence, answer, Subsequence);
  answer hairpin(Subsequence, Subsequence, Subsequence, Subsequence, Subsequence);
  answer bulgeleft(Subsequence, Subsequence, Subsequence, answer, Subsequence, Subsequence);
  answer bulgeright(Subsequence, Subsequence, answer, Subsequence, Subsequence, Subsequence);
  answer iloop(Subsequence, Subsequence, Subsequence, answer, Subsequence, Subsequence, Subsequence);
  answer multiloop_drem(Subsequence, Subsequence, answer, Subsequence, Subsequence);
  answer multiloop_edlr(Subsequence, Subsequence, Subsequence, answer, Subsequence, Subsequence, Subsequence);
  answer multiloop_edl (Subsequence, Subsequence, Subsequence, answer, Subsequence, Subsequence);
  answer multiloop_edr (Subsequence, Subsequence, answer, Subsequence, Subsequence, Subsequence);
  choice [answer] h([answer]);
}

algebra enum auto enum ;

algebra pretty implements stefansAlgebra(alphabet = char, answer = Rope) {
  Rope root(Rope e) {
    return e;
  }
  Rope unpaired(Subsequence ss){
    Rope res;
    append(res, '.', size(ss));
    return res;
  }
  Rope lasthlnoss(Rope e){
    return e;
  }
  Rope lasthlss(Rope e, Subsequence ss){
    Rope res;
    append(res, e);
    append(res, '.', size(ss));
    return res;
  }
  Rope nexthl(Rope e1, Rope e2){
    Rope res;
    append(res, e1);
    append(res, e2);
    return res;
  }
  Rope lastmlnoss(Rope e1, Rope e2){
    Rope res;
    append(res, e1);
    append(res, e2);
    return res;
  }
  Rope lastmlss(Rope e1, Rope e2, Subsequence ss){
    Rope res;
    append(res, e1);
    append(res, e2);
    append(res, '.', size(ss));
    return res;
  }
  Rope nextml(Rope e1, Rope e2) {
    Rope res;
    append(res, e1);
    append(res, e2);
    return res;
  }
  Rope addRegion(Subsequence ss, Rope e){
    Rope res;
    append(res, '.', size(ss));
    append(res, e);
    return res;
  }
  Rope startstem(Rope e){
    return e;
  }
  Rope drem(Subsequence ld, Rope e, Subsequence rd){
    return e;
  }
  Rope edlr(Subsequence ld, Rope e, Subsequence rd){
    Rope res;
    append(res, '>');
    append(res, e);
    append(res, '<');
    return res;
  }
  Rope edl(Subsequence ld, Rope e, Subsequence rd){
    Rope res;
    append(res, '>');
    append(res, e);
    return res;
  }
  Rope edr(Subsequence ld, Rope e, Subsequence rd){
    Rope res;
    append(res, e);
    append(res, '<');
    return res;
  }
  Rope stack(Subsequence lb, Rope e, Subsequence rb){
    Rope res;
    append(res, '(');
    append(res, e);
    append(res, ')');
    return res;
  }
  Rope hairpin(Subsequence llb, Subsequence lb, Subsequence loop, Subsequence rb, Subsequence rrb){
    Rope res;
    append(res, "((", 2);
    append(res, '.', size(loop));
    append(res, "))", 2);
    return res;
  }
  Rope bulgeleft(Subsequence llb, Subsequence lb, Subsequence lr, Rope e, Subsequence rb, Subsequence rrb){
    Rope res;
    append(res, "((", 2);
    append(res, '.', size(lr));
    append(res, e);
    append(res, "))", 2);
    return res;
  }
  Rope bulgeright(Subsequence llb, Subsequence lb, Rope e, Subsequence rr, Subsequence rb, Subsequence rrb){
    Rope res;
    append(res, "((", 2);
    append(res, e);
    append(res, '.', size(rr));
    append(res, "))", 2);
    return res;
  }
  Rope iloop(Subsequence llb, Subsequence lb, Subsequence lr, Rope e, Subsequence rr, Subsequence rb, Subsequence rrb){
    Rope res;
    append(res, "((", 2);
    append(res, '.', size(lr));
    append(res, e);
    append(res, '.', size(rr));
    append(res, "))", 2);
    return res;
  }
  Rope multiloop_drem(Subsequence llb, Subsequence lb, Rope e, Subsequence rb, Subsequence rrb){
    Rope res;
    append(res, "((", 2);
    append(res, e);
    append(res, "))", 2);
    return res;
  }
  Rope multiloop_edlr(Subsequence llb, Subsequence lb, Subsequence ld, Rope e, Subsequence rd, Subsequence rb, Subsequence rrb){
    Rope res;
    append(res, "((", 2);
    append(res, '<');
    append(res, e);
    append(res, '>');
    append(res, "))", 2);
    return res;
  }
  Rope multiloop_edl (Subsequence llb, Subsequence lb, Subsequence ld, Rope e, Subsequence rb, Subsequence rrb){
    Rope res;
    append(res, "((", 2);
    append(res, '<');
    append(res, e);
    append(res, "))", 2);
    return res;
  }
  Rope multiloop_edr (Subsequence llb, Subsequence lb, Rope e, Subsequence rd, Subsequence rb, Subsequence rrb){
    Rope res;
    append(res, "((", 2);
    append(res, e);
    append(res, '>');
    append(res, "))", 2);
    return res;
  }
  choice [Rope] h([Rope] i){
    return i;
  }
}

algebra shape5 implements stefansAlgebra(alphabet = char, answer = shape_t) {
  shape_t root(shape_t e) {
    return e;
  }
  shape_t unpaired(Subsequence ss){
    return '_';
  }
  shape_t lasthlnoss(shape_t e){
    return e;
  }
  shape_t lasthlss(shape_t e, Subsequence ss){
    return e;
  }
  shape_t nexthl(shape_t e1, shape_t e2){
    return e1 + e2;
  }
  shape_t lastmlnoss(shape_t e1, shape_t e2){
    return e1 + e2;
  }
  shape_t lastmlss(shape_t e1, shape_t e2, Subsequence ss){
    return e1 + e2;
  }
  shape_t nextml(shape_t e1, shape_t e2) {
    return e1 + e2;
  }
  shape_t addRegion(Subsequence ss, shape_t e){
    return e;
  }
  shape_t startstem(shape_t e){
    return e;
  }
  shape_t drem(Subsequence ld, shape_t e, Subsequence rd){
    return e;
  }
  shape_t edlr(Subsequence ld, shape_t e, Subsequence rd){
    return e;
  }
  shape_t edl(Subsequence ld, shape_t e, Subsequence rd){
    return e;
  }
  shape_t edr(Subsequence ld, shape_t e, Subsequence rd){
    return e;
  }
  shape_t stack(Subsequence lb, shape_t e, Subsequence rb){
    return e;
  }
  shape_t hairpin(Subsequence llb, Subsequence lb, Subsequence loop, Subsequence rb, Subsequence rrb){
    return shape_t('[') + ']';
  }
  shape_t bulgeleft(Subsequence llb, Subsequence lb, Subsequence lr, shape_t e, Subsequence rb, Subsequence rrb){
    return e;
  }
  shape_t bulgeright(Subsequence llb, Subsequence lb, shape_t e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return e;
  }
  shape_t iloop(Subsequence llb, Subsequence lb, Subsequence lr, shape_t e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return e;
  }
  shape_t multiloop_drem(Subsequence llb, Subsequence lb, shape_t e, Subsequence rb, Subsequence rrb){
    return '[' + e + ']';
  }
  shape_t multiloop_edlr(Subsequence llb, Subsequence lb, Subsequence ld, shape_t e, Subsequence rd, Subsequence rb, Subsequence rrb){
    return '[' + e + ']';
  }
  shape_t multiloop_edl (Subsequence llb, Subsequence lb, Subsequence ld, shape_t e, Subsequence rb, Subsequence rrb){
    return '[' + e + ']';
  }
  shape_t multiloop_edr (Subsequence llb, Subsequence lb, shape_t e, Subsequence rd, Subsequence rb, Subsequence rrb){
    return '[' + e + ']';
  }
  choice [shape_t] h([shape_t] i) {
    return unique(i);
  }
}

algebra shape4 extends shape5 {
  shape_t iloop(Subsequence llb, Subsequence lb, Subsequence lr, shape_t e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return shape_t('[') + e + ']';
  }
}

algebra shape3 extends shape5 {
  shape_t bulgeleft(Subsequence llb, Subsequence lb, Subsequence lr, shape_t e, Subsequence rb, Subsequence rrb){
    return shape_t('[') + e + ']';
  }

  shape_t bulgeright(Subsequence llb, Subsequence lb, shape_t e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return '[' + e + ']';
  }
  
  shape_t iloop(Subsequence llb, Subsequence lb, Subsequence lr, shape_t e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return shape_t('[') + e + ']';
  }
}

algebra shape2 extends shape5 {
  shape_t bulgeleft(Subsequence llb, Subsequence lb, Subsequence lr, shape_t e, Subsequence rb, Subsequence rrb){
    return shape_t('[') + '_' + e + ']';
  }

  shape_t bulgeright(Subsequence llb, Subsequence lb, shape_t e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return '[' + e + '_' + ']';
  }

  shape_t iloop(Subsequence llb, Subsequence lb, Subsequence lr, shape_t e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return shape_t('[') + '_' + e + '_' + ']';
  }
}

algebra shape1 extends shape5 {
  shape_t lasthlss(shape_t e, Subsequence ss){
    if (back(e) == '_') {
      return e;
    } else {
      return e + '_';
    }
  }
  shape_t nexthl(shape_t e1, shape_t e2){
    if (back(e1) == '_' && front(e2) == '_') {
      return e1 + tail(e2);
    } else {
      return e1 + e2;
    }
  }
  shape_t lastmlnoss(shape_t e1, shape_t e2){
    if (back(e1) == '_' && front(e2) == '_') {
      return e1 + tail(e2);
    } else {
      return e1 + e2;
    }
  }
  shape_t lastmlss(shape_t e1, shape_t e2, Subsequence ss){
    shape_t res;
    if (back(e1) == '_' && front(e2) == '_') {
      res = e1 + tail(e2);
    } else {
      res = e1 + e2;
    }
    if (back(res) == '_') {
      return res;
    } else {
      return res + '_';
    }
  }
  shape_t nextml(shape_t e1, shape_t e2) {
    if (back(e1) == '_' && front(e2) == '_') {
      return e1 + tail(e2);
    } else {
      return e1 + e2;
    }
  }
  shape_t addRegion(Subsequence ss, shape_t e){
    if (front(e) == '_') {
      return e;
    } else {
      return '_' + e;
    }
  }
  shape_t edlr(Subsequence ld, shape_t e, Subsequence rd){
    return '_' + e + '_';
  }
  shape_t edl(Subsequence ld, shape_t e, Subsequence rd){
    return '_' + e;
  }
  shape_t edr(Subsequence ld, shape_t e, Subsequence rd){
    return e + '_';
  }
  shape_t bulgeleft(Subsequence llb, Subsequence lb, Subsequence lr, shape_t e, Subsequence rb, Subsequence rrb){
    return shape_t('[') + '_' + e + ']';
  }
  shape_t bulgeright(Subsequence llb, Subsequence lb, shape_t e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return shape_t('[') + e + '_' + ']';
  }
  shape_t iloop(Subsequence llb, Subsequence lb, Subsequence lr, shape_t e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return shape_t('[') + '_' + e + '_' + ']';
  }
  shape_t multiloop_edlr(Subsequence llb, Subsequence lb, Subsequence ld, shape_t e, Subsequence rd, Subsequence rb, Subsequence rrb){
    shape_t res;
    if (front(e) == '_') {
      res = e;
    } else {
      res = '_' + e;
    }
    if (back(e) == '_') {
      res = e;
    } else {
      res = e + '_';
    }
    return '[' + res + ']';
  }
  shape_t multiloop_edl (Subsequence llb, Subsequence lb, Subsequence ld, shape_t e, Subsequence rb, Subsequence rrb){
    shape_t res;
    if (front(e) == '_') {
      res = e;
    } else {
      res = '_' + e;
    }
    return '[' + res + ']';
  }
  shape_t multiloop_edr (Subsequence llb, Subsequence lb, shape_t e, Subsequence rd, Subsequence rb, Subsequence rrb){
    shape_t res;
    if (back(e) == '_') {
      res = e;
    } else {
      res = e + '_';
    }
    return '[' + res + ']';
  }
}
algebra dotbracket extends pretty {
  Rope edlr(Subsequence ld, Rope e, Subsequence rd){
    Rope res;
    append(res, '.');
    append(res, e);
    append(res, '.');
    return res;
  }
  Rope edl(Subsequence ld, Rope e, Subsequence rd){
    Rope res;
    append(res, '.');
    append(res, e);
    return res;
  }
  Rope edr(Subsequence ld, Rope e, Subsequence rd){
    Rope res;
    append(res, e);
    append(res, '.');
    return res;
  }
  Rope multiloop_edlr(Subsequence llb, Subsequence lb, Subsequence ld, Rope e, Subsequence rd, Subsequence rb, Subsequence rrb){
    Rope res;
    append(res, "((", 2);
    append(res, '.');
    append(res, e);
    append(res, '.');
    append(res, "))", 2);
    return res;
  }
  Rope multiloop_edl (Subsequence llb, Subsequence lb, Subsequence ld, Rope e, Subsequence rb, Subsequence rrb){
    Rope res;
    append(res, "((", 2);
    append(res, '.');
    append(res, e);
    append(res, "))", 2);
    return res;
  }
  Rope multiloop_edr (Subsequence llb, Subsequence lb, Rope e, Subsequence rd, Subsequence rb, Subsequence rrb){
    Rope res;
    append(res, "((", 2);
    append(res, e);
    append(res, '.');
    append(res, "))", 2);
    return res;
  }
  choice [Rope] h([Rope] i){
    return unique(i);
  }
}


algebra dummy implements stefansAlgebra(alphabet = char, answer = Rope) {
  Rope root(Rope e) {
    return e;
  }
  Rope unpaired(Subsequence ss){
    return ss;
  }
  Rope lasthlnoss(Rope e){
    return e;
  }
  Rope lasthlss(Rope e, Subsequence ss){
    return e;
  }
  Rope nexthl(Rope e1, Rope e2){
    return e1;
  }
  Rope lastmlnoss(Rope e1, Rope e2){
    return e1;
  }
  Rope lastmlss(Rope e1, Rope e2, Subsequence ss){
    return e1;
  }
  Rope nextml(Rope e1, Rope e2) {
    return e1;
  }
  Rope addRegion(Subsequence ss, Rope e){
    return e;
  }
  Rope startstem(Rope e){
    return e;
  }
  Rope drem(Subsequence ld, Rope e, Subsequence rd){
    return e;
  }
  Rope edlr(Subsequence ld, Rope e, Subsequence rd){
    return e;
  }
  Rope edl(Subsequence ld, Rope e, Subsequence rd){
    return e;
  }
  Rope edr(Subsequence ld, Rope e, Subsequence rd){
    return e;
  }
  Rope stack(Subsequence lb, Rope e, Subsequence rb){
    return e;
  }
  Rope hairpin(Subsequence llb, Subsequence lb, Subsequence loop, Subsequence rb, Subsequence rrb){
    return e;
  }
  Rope bulgeleft(Subsequence llb, Subsequence lb, Subsequence lr, Rope e, Subsequence rb, Subsequence rrb){
    return e;
  }
  Rope bulgeright(Subsequence llb, Subsequence lb, Rope e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return e;
  }
  Rope iloop(Subsequence llb, Subsequence lb, Subsequence lr, Rope e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return e;
  }
  Rope multiloop_drem(Subsequence llb, Subsequence lb, Rope e, Subsequence rb, Subsequence rrb){
    return e;
  }
  Rope multiloop_edlr(Subsequence llb, Subsequence lb, Subsequence ld, Rope e, Subsequence rd, Subsequence rb, Subsequence rrb){
    return e;
  }
  Rope multiloop_edl (Subsequence llb, Subsequence lb, Subsequence ld, Rope e, Subsequence rb, Subsequence rrb){
    return e;
  }
  Rope multiloop_edr (Subsequence llb, Subsequence lb, Rope e, Subsequence rd, Subsequence rb, Subsequence rrb){
    return e;
  }
  choice [Rope] h([Rope] i){
    return i;
  }
}

algebra mfe implements stefansAlgebra(alphabet = char, answer = int) {
  int root(int e) {
    return e;
  }
  int unpaired(Subsequence ss){
    return 0;
  }
  int lasthlnoss(int e){
    return e;
  }
  int lasthlss(int e, Subsequence ss){
    return e;
  }
  int nexthl(int e1, int e2){
    return e1 + e2;
  }
  int lastmlnoss(int e1, int e2){
    return e1 + e2 + 80;
  }
  int lastmlss(int e1, int e2, Subsequence ss){
    return e1 + e2 + 80;
  }
  int nextml(int e1, int e2) {
    return e1 + e2 + 40;
  }
  int addRegion(Subsequence ss, int e){
    return e;
  }
  int startstem(int e){
    return e;
  }
  int drem(Subsequence ld, int e, Subsequence rd){
    return e + termaupenalty(ld, rd);
  }
  int edlr(Subsequence ld, int e, Subsequence rd){
    Subsequence stem;
    stem.seq = ld.seq;
    stem.i = ld.i+1;
    stem.j = rd.j-1;
    return e + termaupenalty(ld, rd) + dl_energy(stem, stem) + dr_energy(stem, stem);
  }
  int edl(Subsequence ld, int e, Subsequence rd){
    Subsequence stem;
    stem.seq = ld.seq;
    stem.i = ld.i+1;
    stem.j = rd.j-1;
    return e + termaupenalty(ld, rd) + dl_energy(stem, stem);
  }
  int edr(Subsequence ld, int e, Subsequence rd){
    Subsequence stem;
    stem.seq = ld.seq;
    stem.i = ld.i+1;
    stem.j = rd.j-1;
    return e + termaupenalty(ld, rd) + dr_energy(stem, stem);
  }
  int stack(Subsequence lb, int e, Subsequence rb){
    return       e + sr_energy(lb, rb);
  }
  int hairpin(Subsequence llb, Subsequence lb, Subsequence loop, Subsequence rb, Subsequence rrb){
    return           sr_energy(llb, rrb) + hl_energy(lb, rb);
  }
  int bulgeleft(Subsequence llb, Subsequence lb, Subsequence lr, int e, Subsequence rb, Subsequence rrb){
    return       e + sr_energy(llb, rrb) + bl_energy(lb, lr, rb);
  }
  int bulgeright(Subsequence llb, Subsequence lb, int e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return       e + sr_energy(llb, rrb) + br_energy(lb, rr, rb);
  }
  int iloop(Subsequence llb, Subsequence lb, Subsequence lr, int e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return       e + sr_energy(llb, rrb) + il_energy(lr, rr);
  }
  int multiloop_drem(Subsequence llb, Subsequence lb, int e, Subsequence rb, Subsequence rrb){
    return 380 + e + sr_energy(llb, rrb) + termaupenalty(lb, rb);
  }
  int multiloop_edlr(Subsequence llb, Subsequence lb, Subsequence ld, int e, Subsequence rd, Subsequence rb, Subsequence rrb){
    return 380 + e + sr_energy(llb, rrb) + termaupenalty(lb, rb) + dli_energy(lb, rb) + dri_energy(lb, rb);
  }
  int multiloop_edl (Subsequence llb, Subsequence lb, Subsequence ld, int e, Subsequence rb, Subsequence rrb){
    return 380 + e + sr_energy(llb, rrb) + termaupenalty(lb, rb) + dli_energy(lb, rb);
  }
  int multiloop_edr (Subsequence llb, Subsequence lb, int e, Subsequence rd, Subsequence rb, Subsequence rrb){
    return 380 + e + sr_energy(llb, rrb) + termaupenalty(lb, rb) + dri_energy(lb, rb);
  }
  choice [int] h([int] i){
    return list(minimum(i));
  }
}

algebra p_func implements stefansAlgebra(alphabet = char, answer = double) {
  double root(double e) {
    return e;
  }
  double unpaired(Subsequence ss){
    return scale(ss.j - ss.i);
  }
  double lasthlnoss(double e){
    return e;
  }
  double lasthlss(double e, Subsequence ss){
    return e * scale(ss.j - ss.i);
  }
  double nexthl(double e1, double e2){
    return e1 * e2;
  }
  double lastmlnoss(double e1, double e2){
    return e1 * e2 * mk_pf(80);
  }
  double lastmlss(double e1, double e2, Subsequence ss){
    return e1 * e2 * mk_pf(80);
  }
  double nextml(double e1, double e2) {
    return e1 * e2 * mk_pf(40);
  }
  double addRegion(Subsequence ss, double e){
    return e * scale(ss.j - ss.i);
  }
  double startstem(double e){
    return e;
  }
  double drem(Subsequence ld, double e, Subsequence rd){
    return            e * mk_pf(termaupenalty(ld, rd));
  }
  double edlr(Subsequence ld, double e, Subsequence rd){
    Subsequence stem;
    stem.seq = ld.seq;
    stem.i = ld.i+1;
    stem.j = rd.j-1;
    return scale(2) * e * mk_pf(termaupenalty(ld, rd) + dl_energy(stem, stem) + dr_energy(stem, stem));
  }
  double edl(Subsequence ld, double e, Subsequence rd){
    Subsequence stem;
    stem.seq = ld.seq;
    stem.i = ld.i+1;
    stem.j = rd.j-1;
    return scale(1) * e * mk_pf(termaupenalty(ld, rd) + dl_energy(stem, stem));
  }
  double edr(Subsequence ld, double e, Subsequence rd){
    Subsequence stem;
    stem.seq = ld.seq;
    stem.i = ld.i+1;
    stem.j = rd.j-1;
    return scale(1) * e * mk_pf(termaupenalty(ld, rd) + dr_energy(stem, stem));
  }
  double stack(Subsequence lb, double e, Subsequence rb){
    return scale(2)                     * e * mk_pf(sr_energy(lb, rb));
  }
  double hairpin(Subsequence llb, Subsequence lb, Subsequence loop, Subsequence rb, Subsequence rrb){
    return scale(4+loop.j-loop.i)       *     mk_pf(sr_energy(llb, rrb) + hl_energy(lb, rb));
  }
  double bulgeleft(Subsequence llb, Subsequence lb, Subsequence lr, double e, Subsequence rb, Subsequence rrb){
    return scale(4+lr.j-lr.i)           * e * mk_pf(sr_energy(llb, rrb) + bl_energy(lb, lr, rb));
  }
  double bulgeright(Subsequence llb, Subsequence lb, double e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return scale(4+rr.j-rr.i)           * e * mk_pf(sr_energy(llb, rrb) + br_energy(lb, rr, rb));
  }
  double iloop(Subsequence llb, Subsequence lb, Subsequence lr, double e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return scale(4+lr.j-lr.i+rr.j-rr.i) * e * mk_pf(sr_energy(llb, rrb) + il_energy(lr, rr));
  }
  double multiloop_drem(Subsequence llb, Subsequence lb, double e, Subsequence rb, Subsequence rrb){
    return scale(4)                     * e * mk_pf(380 + sr_energy(llb, rrb) + termaupenalty(lb, rb));
  }
  double multiloop_edlr(Subsequence llb, Subsequence lb, Subsequence ld, double e, Subsequence rd, Subsequence rb, Subsequence rrb){
    return scale(6)                     * e * mk_pf(380 + sr_energy(llb, rrb) + termaupenalty(lb, rb) + dli_energy(lb, rb) + dri_energy(lb, rb));
  }
  double multiloop_edl (Subsequence llb, Subsequence lb, Subsequence ld, double e, Subsequence rb, Subsequence rrb){
    return scale(5)                     * e * mk_pf(380 + sr_energy(llb, rrb) + termaupenalty(lb, rb) + dli_energy(lb, rb));
  }
  double multiloop_edr (Subsequence llb, Subsequence lb, double e, Subsequence rd, Subsequence rb, Subsequence rrb){
    return scale(5)                     * e * mk_pf(380 + sr_energy(llb, rrb) + termaupenalty(lb, rb) + dri_energy(lb, rb));
  }
  choice [double] h([double] i){
    return list(sum(i));
  }
}

algebra count implements stefansAlgebra(alphabet = char, answer = int) {
  int root(int e) {
    return 22;
  }
  int unpaired(Subsequence ss){
    return 33;
  }
  int lasthlnoss(int e){
    return e;
  }
  int lasthlss(int e, Subsequence ss){
    return e;
  }
  int nexthl(int e1, int e2){
    return e1 * e2;
  }
  int lastmlnoss(int e1, int e2){
    return e1 * e2;
  }
  int lastmlss(int e1, int e2, Subsequence ss){
    return e1 * e2;
  }
  int nextml(int e1, int e2) {
    return e1 * e2;
  }
  int addRegion(Subsequence ss, int e){
    return e;
  }
  int startstem(int e){
    return e;
  }
  int drem(Subsequence ld, int e, Subsequence rd){
    return e;
  }
  int edlr(Subsequence ld, int e, Subsequence rd){
    return e;
  }
  int edl(Subsequence ld, int e, Subsequence rd){
    return e;
  }
  int edr(Subsequence ld, int e, Subsequence rd){
    return e;
  }
  int stack(Subsequence lb, int e, Subsequence rb){
    return e;
  }
  int hairpin(Subsequence llb, Subsequence lb, Subsequence loop, Subsequence rb, Subsequence rrb){
    return 1;
  }
  int bulgeleft(Subsequence llb, Subsequence lb, Subsequence lr, int e, Subsequence rb, Subsequence rrb){
    return e;
  }
  int bulgeright(Subsequence llb, Subsequence lb, int e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return e;
  }
  int iloop(Subsequence llb, Subsequence lb, Subsequence lr, int e, Subsequence rr, Subsequence rb, Subsequence rrb){
    return e;
  }
  int multiloop_drem(Subsequence llb, Subsequence lb, int e, Subsequence rb, Subsequence rrb){
    return e;
  }
  int multiloop_edlr(Subsequence llb, Subsequence lb, Subsequence ld, int e, Subsequence rd, Subsequence rb, Subsequence rrb){
    return e;
  }
  int multiloop_edl (Subsequence llb, Subsequence lb, Subsequence ld, int e, Subsequence rb, Subsequence rrb){
    return e;
  }
  int multiloop_edr (Subsequence llb, Subsequence lb, int e, Subsequence rd, Subsequence rb, Subsequence rrb){
    return e;
  }
  choice [int] h([int] i){
    return list(sum(i));
  }
}

grammar stefansDangle uses stefansAlgebra(axiom = struct) {
  struct = root(hlcons) | 
           unpaired(REGION) 
           # h;
  
  hlcons = lasthlnoss(component)       | 
           lasthlss(component, REGION) | 
           nexthl(component, hlcons) 
           # h;
  
  mlcons = lastmlnoss(component, component)       |
           lastmlss(component, component, REGION) |
           nextml(component, mlcons) 
           # h;
  
  component = addRegion(REGION, initstem) |
              startstem(initstem)
              # h;
  
  initstem = drem(LOC, stem, LOC)   |
             edlr(BASE, stem, BASE) |
             edl(BASE, stem, LOC)   |
             edr(LOC, stem, BASE)
             # h;
        
  stem = {stack         (      BASE,                          stem,                            BASE      ) with basepairing}  |
         {hairpin       (BASE, BASE,                          REGION with minsize(3),          BASE, BASE) with stackpairing} |
         {bulgeleft     (BASE, BASE, REGION,                  stem,                            BASE, BASE) with stackpairing} |
         {bulgeright    (BASE, BASE,                          stem,   REGION,                  BASE, BASE) with stackpairing} |
         {iloop         (BASE, BASE, REGION with maxsize(30), stem,   REGION with maxsize(30), BASE, BASE) with stackpairing} |
         {multiloop_drem(BASE, BASE,                          mlcons,                          BASE, BASE) with stackpairing} |
         {multiloop_edlr(BASE, BASE, BASE,                    mlcons, BASE,                    BASE, BASE) with stackpairing} |
         {multiloop_edl (BASE, BASE, BASE,                    mlcons,                          BASE, BASE) with stackpairing} |
         {multiloop_edr (BASE, BASE,                          mlcons, BASE,                    BASE, BASE) with stackpairing}
         # h;      
}

instance enum = stefansDangle ( enum );
instance count = stefansDangle ( count );
instance pretty = stefansDangle ( pretty );
instance dotbracket = stefansDangle ( dotbracket );
instance shape5 = stefansDangle(shape5);
instance mfepp = stefansDangle(mfe * pretty);
instance ppmfe = stefansDangle(pretty * mfe);
instance shape5mfepp = stefansDangle((shape5 * mfe) * pretty);
//~ instance pf = stefansDangle(shape5 * p_func);

instance shape5pfx = stefansDangle ((shape5 * p_func) suchthat p_func_filter);
instance shape4pfx = stefansDangle ((shape4 * p_func) suchthat p_func_filter);
instance shape3pfx = stefansDangle ((shape3 * p_func) suchthat p_func_filter);
instance shape2pfx = stefansDangle ((shape2 * p_func) suchthat p_func_filter);
instance shape1pfx = stefansDangle ((shape1 * p_func) suchthat p_func_filter);

import rna
import rational
import look
input rna

type shape_t = shape

signature fs(alphabet, answer) {
  answer s1(Subsequence, answer, answer, Subsequence);
  answer s2(Subsequence, answer, alphabet, Subsequence);
  answer s3(Subsequence, alphabet, Subsequence);
  answer s4(Subsequence, answer, Subsequence);

  answer p1(Subsequence, alphabet, answer, alphabet, Subsequence);
  answer p2(Subsequence, alphabet, answer, alphabet, Subsequence);

  answer r1(Subsequence, answer, alphabet, Subsequence);
  answer r2(Subsequence, answer, answer, Subsequence);

  answer t1(Subsequence, answer, alphabet, Subsequence);
  answer t2(Subsequence, answer, answer, Subsequence);
  answer t3(Subsequence, alphabet, Subsequence);
  answer t4(Subsequence, answer, Subsequence);

  choice [answer] h([answer]);
}

algebra bpmax implements fs(alphabet = char, answer = int)
{

  int s1(Subsequence l, int a, int b, Subsequence r)
  {
    return a + b;
  }

  int s2(Subsequence l, int a, alphabet b, Subsequence r)
  {
    return a;
  }

  int s3(Subsequence l, alphabet a, Subsequence r)
  {
    return 0;
  }

  int s4(Subsequence l, int a, Subsequence r)
  {
    return a;
  }

  int p1(Subsequence l, alphabet a, int b, alphabet c, Subsequence r)
  {
    return b + 1;
  }

  int p2(Subsequence l, alphabet a, int b, alphabet c, Subsequence r)
  {
    return b + 1;
  }

  int r1(Subsequence l, int a, alphabet b, Subsequence r)
  {
    return a;
  }

  int r2(Subsequence l, int a, int b, Subsequence r)
  {
    return a + b;
  }

  int t1(Subsequence l, int a, alphabet b, Subsequence r)
  {
    return a;
  }

  int t2(Subsequence l, int a, int b, Subsequence r)
  {
    return a + b;
  }

  int t3(Subsequence l, alphabet a, Subsequence r)
  {
    return 0;
  }

  int t4(Subsequence l, int a, Subsequence r)
  {
    return a;
  }

  choice [int] h([int] l)
  {
    return list(maximum(l));
  }

}

// FIXME use double log space

algebra prob implements fs(alphabet = char, answer = rational)
{

  rational s1(Subsequence l, rational a, rational b, Subsequence r)
  {
    return lookup(S_S_P, l, r) * a * b;
  }

  rational s2(Subsequence l, rational a, alphabet b, Subsequence r)
  {
    return lookup(S_S_O, l, r) * lookup(b) * a;
  }

  rational s3(Subsequence l, alphabet a, Subsequence r)
  {
    return lookup(S_O, l, r) * lookup(a);
  }

  rational s4(Subsequence l, rational a, Subsequence r)
  {
    return lookup(S_P, l, r) * a;
  }

  rational p1(Subsequence l, alphabet a, rational b, alphabet c, Subsequence r)
  {
    return lookup_p1(a, c, l, r) * b;
  }

  rational p2(Subsequence l, alphabet a, rational b, alphabet c, Subsequence r)
  {
    return lookup_p2(a, c, l, r) * b;
  }

  rational r1(Subsequence l, rational a, alphabet b, Subsequence r)
  {
    return lookup(R_T_O, l, r) * lookup(b) * a;
  }

  rational r2(Subsequence l, rational a, rational b, Subsequence r)
  {
    return lookup(R_T_P, l, r) * a * b;
  }

  rational t1(Subsequence l, rational a, alphabet b, Subsequence r)
  {
    return lookup(T_T_O, l, r) * lookup(b) * a;
  }

  rational t2(Subsequence l, rational a, rational b, Subsequence r)
  {
    return lookup(T_T_P, l, r) * a * b;
  }

  rational t3(Subsequence l, alphabet a, Subsequence r)
  {
    return lookup(T_O, l, r) * lookup(a);
  }

  rational t4(Subsequence l, rational a, Subsequence r)
  {
    return lookup(T_P, l, r) * a;
  }

  choice [rational] h([rational] l)
  {
    return list(sum(l));
  }

}

algebra probl implements fs(alphabet = char, answer = double)
{

  double s1(Subsequence l, double a, double b, Subsequence r)
  {
    return lookup(S_S_P, l, r) + a + b;
  }

  double s2(Subsequence l, double a, alphabet b, Subsequence r)
  {
    return lookup(S_S_O, l, r) + lookup(b) + a;
  }

  double s3(Subsequence l, alphabet a, Subsequence r)
  {
    return lookup(S_O, l, r) + lookup(a);
  }

  double s4(Subsequence l, double a, Subsequence r)
  {
    return lookup(S_P, l, r) + a;
  }

  double p1(Subsequence l, alphabet a, double b, alphabet c, Subsequence r)
  {
    return lookup_p1(a, c, l, r) + b;
  }

  double p2(Subsequence l, alphabet a, double b, alphabet c, Subsequence r)
  {
    return lookup_p2(a, c, l, r) + b;
  }

  double r1(Subsequence l, double a, alphabet b, Subsequence r)
  {
    return lookup(R_T_O, l, r) + lookup(b) + a;
  }

  double r2(Subsequence l, double a, double b, Subsequence r)
  {
    return lookup(R_T_P, l, r) + a + b;
  }

  double t1(Subsequence l, double a, alphabet b, Subsequence r)
  {
    return lookup(T_T_O, l, r) + lookup(b) + a;
  }

  double t2(Subsequence l, double a, double b, Subsequence r)
  {
    return lookup(T_T_P, l, r) + a + b;
  }

  double t3(Subsequence l, alphabet a, Subsequence r)
  {
    return lookup(T_O, l, r) + lookup(a);
  }

  double t4(Subsequence l, double a, Subsequence r)
  {
    return lookup(T_P, l, r) + a;
  }

  choice [double] h([double] l)
  {
    return list(expsum(l));
  }

}

algebra probd implements fs(alphabet = char, answer = double)
{

  double s1(Subsequence l, double a, double b, Subsequence r)
  {
    return lookup(S_S_P, l, r) * a * b;
  }

  double s2(Subsequence l, double a, alphabet b, Subsequence r)
  {
    return lookup(S_S_O, l, r) * lookup(b) * a;
  }

  double s3(Subsequence l, alphabet a, Subsequence r)
  {
    return lookup(S_O, l, r) * lookup(a);
  }

  double s4(Subsequence l, double a, Subsequence r)
  {
    return lookup(S_P, l, r) * a;
  }

  double p1(Subsequence l, alphabet a, double b, alphabet c, Subsequence r)
  {
    return lookup_p1(a, c, l, r) * b;
  }

  double p2(Subsequence l, alphabet a, double b, alphabet c, Subsequence r)
  {
    return lookup_p2(a, c, l, r) * b;
  }

  double r1(Subsequence l, double a, alphabet b, Subsequence r)
  {
    return lookup(R_T_O, l, r) * lookup(b) * a;
  }

  double r2(Subsequence l, double a, double b, Subsequence r)
  {
    return lookup(R_T_P, l, r) * a * b;
  }

  double t1(Subsequence l, double a, alphabet b, Subsequence r)
  {
    return lookup(T_T_O, l, r) * lookup(b) * a;
  }

  double t2(Subsequence l, double a, double b, Subsequence r)
  {
    return lookup(T_T_P, l, r) * a * b;
  }

  double t3(Subsequence l, alphabet a, Subsequence r)
  {
    return lookup(T_O, l, r) * lookup(a);
  }

  double t4(Subsequence l, double a, Subsequence r)
  {
    return lookup(T_P, l, r) * a;
  }

  choice [double] h([double] l)
  {
    return list(sum(l));
  }

}

algebra probl_scale implements fs(alphabet = char, answer = double)
{

  double s1(Subsequence l, double a, double b, Subsequence r)
  {
    return lookup(S_S_P, l, r) + a + b;
  }

  double s2(Subsequence l, double a, alphabet b, Subsequence r)
  {
    return lookup(S_S_O, l, r) + lookup(b) + a - scale_l(1);
  }

  double s3(Subsequence l, alphabet a, Subsequence r)
  {
    return lookup(S_O, l, r) + lookup(a) - scale_l(1);
  }

  double s4(Subsequence l, double a, Subsequence r)
  {
    return lookup(S_P, l, r) + a;
  }

  double p1(Subsequence l, alphabet a, double b, alphabet c, Subsequence r)
  {
    return lookup_p1(a, c, l, r) + b - scale_l(2);
  }

  double p2(Subsequence l, alphabet a, double b, alphabet c, Subsequence r)
  {
    return lookup_p2(a, c, l, r) + b - scale_l(2);
  }

  double r1(Subsequence l, double a, alphabet b, Subsequence r)
  {
    return lookup(R_T_O, l, r) + lookup(b) + a - scale_l(1);
  }

  double r2(Subsequence l, double a, double b, Subsequence r)
  {
    return lookup(R_T_P, l, r) + a + b;
  }

  double t1(Subsequence l, double a, alphabet b, Subsequence r)
  {
    return lookup(T_T_O, l, r) + lookup(b) + a - scale_l(1);
  }

  double t2(Subsequence l, double a, double b, Subsequence r)
  {
    return lookup(T_T_P, l, r) + a + b;
  }

  double t3(Subsequence l, alphabet a, Subsequence r)
  {
    return lookup(T_O, l, r) + lookup(a) - scale_l(1);
  }

  double t4(Subsequence l, double a, Subsequence r)
  {
    return lookup(T_P, l, r) + a;
  }

  choice [double] h([double] l)
  {
    return list(expsum(l));
  }

}

algebra probd_scale implements fs(alphabet = char, answer = double)
{

  double s1(Subsequence l, double a, double b, Subsequence r)
  {
    return lookup(S_S_P, l, r) * a * b;
  }

  double s2(Subsequence l, double a, alphabet b, Subsequence r)
  {
    return lookup(S_S_O, l, r) * lookup(b) * a * scale_d(1);
  }

  double s3(Subsequence l, alphabet a, Subsequence r)
  {
    return lookup(S_O, l, r) * lookup(a) * scale_d(1);
  }

  double s4(Subsequence l, double a, Subsequence r)
  {
    return lookup(S_P, l, r) * a;
  }

  double p1(Subsequence l, alphabet a, double b, alphabet c, Subsequence r)
  {
    return lookup_p1(a, c, l, r) * b * scale_d(2);
  }

  double p2(Subsequence l, alphabet a, double b, alphabet c, Subsequence r)
  {
    return lookup_p2(a, c, l, r) * b * scale_d(2);
  }

  double r1(Subsequence l, double a, alphabet b, Subsequence r)
  {
    return lookup(R_T_O, l, r) * lookup(b) * a * scale_d(1);
  }

  double r2(Subsequence l, double a, double b, Subsequence r)
  {
    return lookup(R_T_P, l, r) * a * b;
  }

  double t1(Subsequence l, double a, alphabet b, Subsequence r)
  {
    return lookup(T_T_O, l, r) * lookup(b) * a * scale_d(1);
  }

  double t2(Subsequence l, double a, double b, Subsequence r)
  {
    return lookup(T_T_P, l, r) * a * b;
  }

  double t3(Subsequence l, alphabet a, Subsequence r)
  {
    return lookup(T_O, l, r) * lookup(a) * scale_d(1);
  }

  double t4(Subsequence l, double a, Subsequence r)
  {
    return lookup(T_P, l, r) * a;
  }

  choice [double] h([double] l)
  {
    return list(sum(l));
  }

}



algebra shape5 implements fs(alphabet = char, answer = shape_t)
{

  shape_t s1(Subsequence l, shape_t a, shape_t b, Subsequence r)
  {
    shape_t x;
    append(x, a);
    append(x, b);
    return x;
  }

  shape_t s2(Subsequence l, shape_t a, alphabet b, Subsequence r)
  {
    return a;
  }

  shape_t s3(Subsequence l, alphabet a, Subsequence r)
  {
    shape_t x;
    return x;
  }

  shape_t s4(Subsequence l, shape_t a, Subsequence r)
  {
    return a;
  }

  shape_t p1(Subsequence l, alphabet a, shape_t b, alphabet c, Subsequence r)
  {
    return b;
  }

  shape_t p2(Subsequence l, alphabet a, shape_t b, alphabet c, Subsequence r)
  {
    return b;
/*
    shape_t x;
    append(x, '[');
    append(x, b);
    append(x, ']');
    return x;
*/
  }

  shape_t r1(Subsequence l, shape_t a, alphabet b, Subsequence r)
  {
    shape_t x;
    append(x, "[]", 2);
    return x;
/*
    return a;
*/
  }

  shape_t r2(Subsequence l, shape_t a, shape_t b, Subsequence r)
  {
    shape_t t;
    if (a == t)
      return b;
    shape_t x;
    append(x, '[');
    append(x, a);
    append(x, b);
    append(x, ']');
    return x;

/*
    append(x, a);
    append(x, b);
    return x;
*/
  }

  shape_t t1(Subsequence l, shape_t a, alphabet b, Subsequence r)
  {
    return a;
  }

  shape_t t2(Subsequence l, shape_t a, shape_t b, Subsequence r)
  {
    shape_t x;
    append(x, a);
    append(x, b);
    return x;
  }

  shape_t t3(Subsequence l, alphabet a, Subsequence r)
  {
    shape_t x;
    return x;
  }

  shape_t t4(Subsequence l, shape_t a, Subsequence r)
  {
    return a;
  }

  choice [shape_t] h([shape_t] l)
  {
    return unique(l);
  }

}


grammar g3 uses fs(axiom = struct) {

  struct = s1(LOC, struct, pair, LOC) |
           s2(LOC, struct, CHAR, LOC) |
           s3(LOC, CHAR, LOC) |
           s4(LOC, pair, LOC) # h ;

  pair = { p1(LOC, CHAR, pair, CHAR, LOC) |
           p2(LOC, CHAR, r, CHAR, LOC) } with basepairing # h ;

  r = r1(LOC, t, CHAR, LOC) |
      r2(LOC, t, pair, LOC) # h ;

  t = t1(LOC, t, CHAR, LOC) |
      t2(LOC, t, pair, LOC) |
      t3(LOC, CHAR, LOC) |
      t4(LOC, pair, LOC) # h ;

}

instance shape5 = g3(shape5);
instance shapebpmax = g3(shape5 * bpmax);
instance bpmax = g3(bpmax);

instance prob = g3(prob);
instance shapeprob = g3(shape5*prob);
instance probl = g3(probl);
instance shapeprobl = g3(shape5*probl);

instance probd = g3(probd);
instance shapeprobd = g3(shape5*probd);

instance probls = g3(probl_scale);
instance shapeprobls = g3(shape5*probl_scale);

instance probds = g3(probd_scale);
instance shapeprobds = g3(shape5*probd_scale);


import rna

input rna

//type shape_t = string
type shape_t = shape

signature FS (alphabet, comp) {
  comp sadd(Subsequence, comp);
  comp cadd(comp, comp);
  comp dlr(Subsequence, comp, Subsequence);
  comp sr(Subsequence, comp, Subsequence);
  comp hl(Subsequence, Subsequence, Subsequence, Subsequence, Subsequence);
  comp bl(Subsequence, Subsequence, Subsequence, comp, Subsequence, Subsequence);
  comp br(Subsequence, Subsequence, comp, Subsequence, Subsequence, Subsequence);
  comp il(Subsequence, Subsequence, Subsequence, comp, Subsequence, Subsequence, Subsequence);
  comp ml(Subsequence, Subsequence, comp, Subsequence, Subsequence);

  comp app(comp, comp);
  comp ul(comp);
  comp addss(comp, Subsequence);
  comp ssadd(Subsequence, comp);

  comp nil(void);

  choice [comp] h([comp]);
  choice [comp] ha([comp]);
}


algebra count auto count ;

algebra enum auto enum ;

algebra pretty implements FS(alphabet = char, comp = string)
{

  string sadd(Subsequence lb, string e) {
    string res;
    append(res, '.');
    append(res, e);
    return res;
  }

  string cadd(string x, string e) {
    string res;
    append(res, x);
    append(res, e);
    return res;
  }

  string dlr(Subsequence lb, string e, Subsequence rb) {
    return e;
  }

  string sr(Subsequence lb, string e, Subsequence rb) {
    string r;
    append(r, '(');
    append(r, e);
    append(r, ')');
    return r;
  }

  string hl(Subsequence lb, Subsequence f1, Subsequence x, Subsequence f2, Subsequence rb) {
    string r;
    append(r, "((", 2);
    append(r, '.', size(x));
    append(r, "))", 2);
    return r;
  }

  string bl(Subsequence bl, Subsequence f1, Subsequence x, string e, Subsequence f2, Subsequence br) {
    string r;
    append(r, "((", 2);
    append(r, '.', size(x));
    append(r, e);
    append(r, "))", 2);
    return r;
  }

  string br(Subsequence bl, Subsequence f1, string e, Subsequence x, Subsequence f2, Subsequence br) {
    string r;
    append(r, "((", 2);
    append(r, e);
    append(r, '.', size(x));
    append(r, "))", 2);
    return r;
  }

  string il(Subsequence f1, Subsequence f2, Subsequence r1, string x, Subsequence r2, Subsequence f3, Subsequence f4) {
    string r;
    append(r, "((", 2);
    append(r, '.', size(r1));
    append(r, x);
    append(r, '.', size(r2));
    append(r, "))", 2);
    return r;
  }

  string ml(Subsequence bl, Subsequence f1, string x, Subsequence f2, Subsequence br) {
    string r;
    append(r, "((", 2);
    append(r, x);
    append(r, "))", 2);
    return r;
  }

  string app(string c1, string c) {
    string r;
    append(r, c1);
    append(r, c);
    return r;
  }

  string ul(string c1) {
    return c1;
  }

  string addss(string c1, Subsequence e) {
    string r;
    append(r, c1);
    append(r, '.', size(e));
    return r;
  }

  string ssadd(Subsequence e, string x) {
    string r;
    append(r, '.', size(e));
    append(r, x);
    return r;
  }

  string nil(void) {
    string r;
    return r;
  }

  choice [string] h([string] i)
  {
    return i;
  }

  choice [string] ha([string] i)
  {
    return i;
  }

}


algebra bpmax implements FS(alphabet = char, comp = int)
{

  int sadd(Subsequence lb, int e) {
    return e;
  }

  int cadd(int x, int e) {
    return x + e;
  }

  int dlr(Subsequence lb, int e, Subsequence rb) {
    return e;
            
  }

  int sr(Subsequence lb, int e, Subsequence rb) {
    return e + 1;
  }

  int hl(Subsequence lb, Subsequence f1, Subsequence x,
         Subsequence f2, Subsequence rb) {
    return 2;
  }

  int bl(Subsequence bl, Subsequence f1, Subsequence x,
         int e, Subsequence f2, Subsequence br) {
    return e + 2;
  }

  int br(Subsequence bl, Subsequence f1, int e, Subsequence x,
         Subsequence f2, Subsequence br) {
    return e + 2;
  }

  int il(Subsequence f1, Subsequence f2, Subsequence r1, int x,
         Subsequence r2, Subsequence f3, Subsequence f4) {
    return x + 2;
  }

  int ml(Subsequence bl, Subsequence f1, int x, Subsequence f2, Subsequence br) {
    return x + 2;
  }

  int app(int c1, int c) {
    return c1 + c;
  }

  int ul(int c1) {
    return c1;
  }

  int addss(int c1, Subsequence e) {
    return c1;
  }

  int ssadd(Subsequence e, int x) {
    return x;
  }

  int nil(void) {
    return 0;
  }

  choice [int] h([int] i)
  {
    return list(maximum(i));
  }

  kscoring choice [int] ha([int] i)
  {
    return list(maximum(i));
  }

}


grammar fold uses FS(axiom = struct) {

  tabulated {
    struct, closed, ml_comps, ml_comps1 }

  struct = sadd(BASE, struct)   |
           cadd(dangle, struct) |
           nil(EMPTY) # h ;

  dangle = dlr(LOC, closed, LOC) ;

  closed = { stack | hairpin | leftB | rightB | iloop | multiloop }
           with stackpairing # h ;

  stack = sr(BASE, closed, BASE) ;

  hairpin = hl(BASE, BASE, { REGION with minsize(3) }, BASE, BASE) ;

  leftB = bl( BASE, BASE, REGION with maxsize(30), closed, BASE, BASE) # h ;

  rightB = br( BASE, BASE, closed, REGION with maxsize(30), BASE, BASE) # h ;

  iloop = il( BASE, BASE, REGION with maxsize(30), closed,
              REGION with maxsize(30), BASE, BASE) # h ;

  multiloop = ml( BASE, BASE, ml_comps, BASE, BASE) ;

  ml_comps = sadd(BASE, ml_comps) |
             app( { ul(dangle) } , ml_comps1) # h ;

  ml_comps1 = sadd(BASE, ml_comps1) |
              app(  ul(dangle)  , ml_comps1) |
              ul(dangle) |
              addss( ul(dangle), REGION)  # ha ;


}

instance count = fold ( count ) ;
instance enu = fold ( enum ) ;

instance pretty = fold ( pretty ) ;

instance bpmax = fold ( bpmax ) ;

instance bpmaxpp = fold ( bpmax * pretty ) ;




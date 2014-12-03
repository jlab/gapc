import rna
import adpf_filter

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
}

synoptic algebra icount implements FS(alphabet = char, comp = int)
{

  // typ ist ja eigentlich klar, Vorteil: typechecking, Benuterkontrolle,
  //    codeselbstdokumentation (nicht Zwang immer zur Signatur hochzuscrollen),
  //    stub durch IDE generierbar ...
  int sadd(Subsequence lb, int e) {
    return e;
  }

  int cadd(int x, int e) {
    return x * e;
  }

  int dlr(Subsequence lb, int e, Subsequence rb) {
    return e;
  }

  int sr(Subsequence lb, int e, Subsequence rb) {
    return e;
  }

  int hl(Subsequence lb, Subsequence f1, Subsequence r, Subsequence f2, Subsequence rb) {
    return 1;
  }

  int bl(Subsequence bl, Subsequence f1, Subsequence x, int e, Subsequence f2, Subsequence br) {
    return e;
  }

  int br(Subsequence bl, Subsequence f1, int e, Subsequence x, Subsequence f2, Subsequence br) {
    return e;
  }

  int il(Subsequence f1, Subsequence f2, Subsequence r1, int x, Subsequence r2, Subsequence f3, Subsequence f4) {
    return x;
  }

  int ml(Subsequence bl, Subsequence f1, int x, Subsequence f2, Subsequence br) {
    return x;
  }

  int app(int c1, int c) {
    return c1 * c;
  }

  int ul(int c1) {
    return c1;
  }

  int addss(int c1, Subsequence r) {
    return c1;
  }

  int ssadd(Subsequence r, int x) {
    return x;
  }

  int nil(void) {
    return 1;
  }

  choice [int] h([int] i)
  {
    return list(sum(i));
  }


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

}

algebra shape5 implements FS(alphabet = char, comp = shape_t)
{

  shape_t sadd(Subsequence lb, shape_t e) {
    return e;
  }

  shape_t cadd(shape_t x, shape_t e) {
    shape_t res;
    append(res, x);
    append(res, e);
    return res;
  }

  shape_t dlr(Subsequence lb, shape_t e, Subsequence rb) {
    return e;
  }

  shape_t sr(Subsequence lb, shape_t e, Subsequence rb) {
    return e;
  }

  shape_t hl(Subsequence lb, Subsequence f1, Subsequence x, Subsequence f2, Subsequence rb) {
    shape_t r;
    append(r, "[]", 2);
    return r;
  }

  shape_t bl(Subsequence bl, Subsequence f1, Subsequence x, shape_t e, Subsequence f2, Subsequence br) {
    return e;
  }

  shape_t br(Subsequence bl, Subsequence f1, shape_t e, Subsequence x, Subsequence f2, Subsequence br) {
    return e;
  }

  shape_t il(Subsequence f1, Subsequence f2, Subsequence r1, shape_t x, Subsequence r2, Subsequence f3, Subsequence f4) {
    return x;
  }

  shape_t ml(Subsequence bl, Subsequence f1, shape_t x, Subsequence f2, Subsequence br) {
    shape_t r;
    append(r, '[');
    append(r, x);
    append(r, ']');
    return r;
  }

  shape_t app(shape_t c1, shape_t c) {
    shape_t r;
    append(r, c1);
    append(r, c);
    return r;
  }

  shape_t ul(shape_t c1) {
    return c1;
  }

  shape_t addss(shape_t c1, Subsequence e) {
    return c1;
  }

  shape_t ssadd(Subsequence e, shape_t x) {
    return x;
  }

  shape_t nil(void) {
    shape_t r;
    return r;
  }

  choice [shape_t] h([shape_t] i)
  {
    return unique(i);
//    return i;
  }

}

algebra shape5str implements FS(alphabet = char, comp = string)
{

  string sadd(Subsequence lb, string e) {
    return e;
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
    return e;
  }

  string hl(Subsequence lb, Subsequence f1, Subsequence x, Subsequence f2, Subsequence rb) {
    string r;
    append(r, "[]", 2);
    return r;
  }

  string bl(Subsequence bl, Subsequence f1, Subsequence x, string e, Subsequence f2, Subsequence br) {
    return e;
  }

  string br(Subsequence bl, Subsequence f1, string e, Subsequence x, Subsequence f2, Subsequence br) {
    return e;
  }

  string il(Subsequence f1, Subsequence f2, Subsequence r1, string x, Subsequence r2, Subsequence f3, Subsequence f4) {
    return x;
  }

  string ml(Subsequence bl, Subsequence f1, string x, Subsequence f2, Subsequence br) {
    string r;
    append(r, '[');
    append(r, x);
    append(r, ']');
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
    return c1;
  }

  string ssadd(Subsequence e, string x) {
    return x;
  }

  string nil(void) {
    string r;
    return r;
  }

  choice [string] h([string] i)
  {
    return unique(i);
//    return i;
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

}

algebra mfe implements FS(alphabet = char, comp = int)
{

  int sadd(Subsequence lb, int e) {
    return e + sbase_energy();
  }

  int cadd(int x, int e) {
    return x + e;
  }

  int dlr(Subsequence lb, int e, Subsequence rb) {
    return e + ext_mismatch_energy(lb, rb)
             + termau_energy(lb, rb);
  }

  int sr(Subsequence lb, int e, Subsequence rb) {
    return e + sr_energy(lb, rb);
  }

  int hl(Subsequence lb, Subsequence f1, Subsequence x,
         Subsequence f2, Subsequence rb) {
    return hl_energy(x) + sr_energy(lb, rb);
  }

  int bl(Subsequence bl, Subsequence f1, Subsequence x,
         int e, Subsequence f2, Subsequence br) {
    return e + bl_energy(x, f2) + sr_energy(bl, br);
  }

  int br(Subsequence bl, Subsequence f1, int e, Subsequence x,
         Subsequence f2, Subsequence br) {
    return e + br_energy(f1, x) + sr_energy(bl, br);
  }

  int il(Subsequence f1, Subsequence f2, Subsequence r1, int x,
         Subsequence r2, Subsequence f3, Subsequence f4) {
    return x + il_energy(r1, r2) + sr_energy(f1, f4);
  }

  int ml(Subsequence bl, Subsequence f1, int x, Subsequence f2, Subsequence br) {
    return ml_energy() + ul_energy() + x + termau_energy(f1, f2) + sr_energy(bl, br)
         + ml_mismatch_energy(f1, f2);
  }

  int app(int c1, int c) {
    return c1 + c;
  }

  int ul(int c1) {
    return ul_energy() + c1;
  }

  int addss(int c1, Subsequence e) {
    return c1 + ss_energy(e);
  }

  int ssadd(Subsequence e, int x) {
    return ul_energy() + x + ss_energy(e);
  }

  int nil(void) {
    return 0;
  }

  choice [int] h([int] i)
  {
    return list(minimum(i));
  }

}

algebra p_func implements FS(alphabet = char, comp = double)
{

  double sadd(Subsequence lb, double e) {
    return scale(1) * e;
  }

  double cadd(double x, double e) {
    return x * e;
  }

  double dlr(Subsequence lb, double e, Subsequence rb) {
    return e * mk_pf(ext_mismatch_energy(lb,rb) + termau_energy(lb,rb));
  }

  double sr(Subsequence lb, double e, Subsequence rb) {
    return scale(2) * e * mk_pf(sr_energy(lb, rb));
  }

  double hl(Subsequence lb, Subsequence f1, Subsequence x, Subsequence f2, Subsequence rb) {
    return scale(x.j - x.i + 4) * mk_pf(hl_energy(x)) * mk_pf(sr_energy(lb,rb));
  }

  double bl(Subsequence bl, Subsequence f1, Subsequence x, double e, Subsequence f2, Subsequence br) {
    return scale(x.j - x.i + 4) * e * mk_pf(bl_energy(x,  f2)) * mk_pf(sr_energy(bl, br));
  }

  double br(Subsequence bl, Subsequence f1, double e, Subsequence x, Subsequence f2, Subsequence br) {
    return scale(x.j - x.i + 4) * e * mk_pf(br_energy(f1, x)) * mk_pf(sr_energy(bl, br));
  }

  double il(Subsequence f1, Subsequence f2, Subsequence r1, double x, Subsequence r2, Subsequence f3, Subsequence f4) {
    return scale((r1.j - r1.i) + (r2.j - r2.i) + 4) * x * mk_pf(il_energy(r1, r2)) * mk_pf(sr_energy(f1, f4));
  }

  double ml(Subsequence bl, Subsequence f1, double x, Subsequence f2, Subsequence br) {
    return scale(4) * x * mk_pf(ml_energy() + ul_energy() + termau_energy(f1, f2) + ml_mismatch_energy(f1, f2)) * mk_pf(sr_energy(bl, br));
  }

  double app(double c1, double c) {
    return c1 * c;
  }

  double ul(double c1) {
    return c1 * mk_pf(ul_energy());
  }

  double addss(double c1, Subsequence e) {
    return scale(e.j - e.i) * c1 * mk_pf(ss_energy(e));
  }

  double ssadd(Subsequence e, double x) {
    return scale(e.j - e.i) * x * mk_pf(ul_energy() + ss_energy(e));
  }

  double nil(void) {
    return 1.0;
  }

  choice [double] h([double] i)
  {
    return list(sum(i));
  }

}

// FIXME how high is the possibility that answer list contain one
// pf-value multiple times?!?

algebra p_func_sample extends p_func {
  scoring choice [double] h([double] l)
  {
    return list(sample_value(l));
  }
}

algebra p_func_id extends p_func {
  choice [double] h([double] l)
  {
    return l;
  }
}

grammar fold uses FS(axiom = struct) {

  tabulated {
    struct, closed, ml_comps, ml_comps1 }

  struct = sadd(BASE, struct)   |

// for(      unsigned int t_0_k_0 = (t_0_i + 7); (t_0_k_0 <= t_0_right_most); ++t_0_k_0)

           .[
             for (int k=t_0_i + 7; k<=t_0_right_most;  k=k+1) {
               int uergs = k;
               INNER(CODE);
             }
           ]. {
             cadd(REGION, REGION) .{
               cadd(dangle[t_0_i, uergs], struct[k])
             }.
           } 
           //cadd(dangle, struct)
           |
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
              addss( ul(dangle), REGION)  # h ;


}

instance count = fold ( count ) ;
instance icount = fold ( icount ) ;

instance pretty = fold ( pretty ) ;

instance enu = fold ( enum ) ;

instance ppen = fold ( pretty * enum ) ;

instance ppenmfe = fold ( pretty * enum * mfe ) ;
instance ppmfe = fold ( pretty * mfe ) ;

instance mfe = fold ( mfe ) ;

instance mfepp = fold ( mfe * pretty ) ;
instance mfeppen = fold ( mfe * pretty * enum ) ;
instance mfeppenum = fold ( mfe * ( pretty * enum )  ) ;
instance mfeen = fold ( mfe * enum ) ;

instance shape5 = fold ( shape5 ) ;
instance shape5str = fold ( shape5str ) ;
instance shapemfepp = fold ( (shape5 * mfe) * pretty ) ;

instance shapemfe = fold (shape5 * mfe) ;

instance bpmax = fold ( bpmax ) ;
instance bpmaxpp = fold ( bpmax * pretty ) ;
instance bpmaxppen = fold ( bpmax * (pretty * enum) ) ;

// instance bar = fold ( shapes(k = 5) * pfunc ) ;


instance pf = fold ( p_func ) ;
//instance pfsample = fold ( (p_func | p_func_sample ) * pretty ) ;
instance pfsamplepp = fold ( ( (p_func | p_func_id ) * pretty )
                                                    suchthat sample_filter ) ;
instance pfsampleshapepp = fold ( ( (p_func | p_func_id ) * (shape5*pretty) )
                                                    suchthat sample_filter ) ;
instance pfsampleshape = fold ( ( (p_func | p_func_id ) * shape5 )
                                                    suchthat sample_filter ) ;

instance shapepf = fold ( shape5 * p_func ) ;

instance shape5pfx = fold ((shape5 * p_func)
                                     suchthat p_func_filter);

//instance shapepp = fold ( shape5*pretty ) ;

instance cart = fold ( bpmax % count ) ;
// violates bp but translates:
instance cartpp = fold ( (bpmax % mfe) * pretty ) ;
// error
//instance carterr = fold ( pretty % count ) ;

instance cartpp2 = fold ( (bpmax % count) * pretty ) ;

instance shapemfepf = fold ( shape5 * ( mfe % p_func ) * pretty ) ;


// if ((tupel.first.first == elem.first && tupel.first.second.first == elem.second.first))


instance shapemfeppcl = fold ( (shape5 / mfe) * pretty ) ;

instance shapemfeppshcl = fold ( (shape5 / mfe) * (pretty*shape5) ) ;

instance prettyshape = fold ( pretty * shape5 ) ;




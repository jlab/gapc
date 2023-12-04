import rna
import typesRNAfolding
import singlefold //necessary to redefine the meaning of the filter "basepair". In singlefold this filter directly calles the build-in "basepairing" filter, in alignmentfold it gets hard codes parameters and returns true or false with dependance to the number of gaps in the rows

input rna

type shape_t = shape
type base_t = extern
type Rope = extern

signature sig_foldrna(alphabet,answer) {
	answer sadd(Subsequence,answer); //add one unpaired base
	answer cadd(answer,answer); //adds one component, which has dangling bases from both sides, next component has a dangling base from left
	answer cadd_Pr(answer,answer); //add one component, which has just a dangling base from left but no dangling base from left to next component
	answer cadd_Pr_Pr(answer,answer); //add one component, which has just a dangling base from right + there is a dangling base from left to next component
	answer cadd_Pr_Pr_Pr(answer,answer); //add one component, with no dangling bases and no dangling base from left to next component
	answer ambd(answer,Subsequence,answer); //add one component
	answer ambd_Pr(answer,Subsequence,answer); //add one component
	answer nil(Subsequence); //empty structure
	answer edl(Subsequence,answer,Subsequence); //dangle left base onto a component
	answer edr(Subsequence,answer,Subsequence); //dangle right base onto a component
	answer edlr(Subsequence,answer,Subsequence); //dangle left and right base onto a component
	answer drem(Subsequence,answer,Subsequence); //no dangle, just the component
	answer sr(Subsequence,answer,Subsequence); //elongate a stack by one base-pair
	answer hl(Subsequence,Subsequence,Subsequence); //a hairpin loop with a closing base-pair
	answer bl(Subsequence, Subsequence, answer, Subsequence); // a bulge loop to the left with a closing base-pair
	answer br(Subsequence, answer, Subsequence, Subsequence); // a bulge loop to the right with a closing base-pair
	answer il(Subsequence, Subsequence, answer, Subsequence, Subsequence); // an internal loop with a closing base-pair
	answer ml(Subsequence,answer,Subsequence);  // a multi-loop with a closing base-pair and no dangling bases
	answer mldr(Subsequence,answer,Subsequence,Subsequence); // a multi-loop with a closing base-pair, inner right base dangles to closing stem
	answer mladr(Subsequence,answer,Subsequence,Subsequence); // a multi-loop with a closing base-pair, inner right base either dangles to last multi-loop stem OR closing stem
	answer mldlr(Subsequence,Subsequence,answer,Subsequence,Subsequence); // a multi-loop with a closing base-pair, both inner bases dangle to closing stem
	answer mladlr(Subsequence,Subsequence,answer,Subsequence,Subsequence); // a multi-loop with a closing base-pair, inner left and right bases both either dangle to closing OR first and second multi-loop stem, respectively
	answer mldladr(Subsequence,Subsequence,answer,Subsequence,Subsequence); // a multi-loop with a closing base-pair, inner left base dangles to closing stem and inner right base dangles either to last multi-stem OR closing stem
	answer mladldr(Subsequence,Subsequence,answer,Subsequence,Subsequence); // a multi-loop with a closing base-pair, inner left base dangles to either to first multi-loop OR closing stem and inner right base to closing stem
	answer mldl(Subsequence,Subsequence,answer,Subsequence); // a multi-loop with a closing base-pair, inner left base dangles to closing stem
	answer mladl(Subsequence,Subsequence,answer,Subsequence); // a multi-loop with a closing base-pair, inner left base dangles to either to first multi-loop OR closing stem
	answer addss(answer,Subsequence); // append a region of unpaired bases
	answer ssadd(Subsequence,answer); // add a region of unpaired bases
	answer trafo(answer); // do some internal transformation
	answer incl(answer); // add penalty for one more multi-loop component
	answer combine(answer,answer); // add one multi-loop component
	answer acomb(answer,Subsequence,answer); // add one multi-loop component
	choice [answer] h([answer]);
}

algebra alg_shape5 implements sig_foldrna(alphabet = char, answer = shape_t) {
  shape_t sadd(Subsequence b, shape_t e) {
    shape_t emptyShape;
    
    if (e == emptyShape) {
      return '_' + e;
    } else {
      return e;
    }
  }

  shape_t cadd(shape_t le,shape_t re) {
    if (re == '_') {
      return le;
    } else {
      return le + re;
    }
  }

  shape_t cadd_Pr(shape_t le,shape_t re) {
    return le + re;
  }

  shape_t cadd_Pr_Pr(shape_t le,shape_t re) {
    if (re == '_') {
      return le;
    } else {
      return le + re;
    }
  }

  shape_t cadd_Pr_Pr_Pr(shape_t le,shape_t re) {
    return le + re;
  }

  shape_t ambd(shape_t le,Subsequence b,shape_t re) {
    return le + re;
  }

  shape_t ambd_Pr(shape_t le,Subsequence b,shape_t re) {
    return le + re;
  }

  shape_t nil(Subsequence loc) {
    shape_t r;
    return r;
  }

  shape_t edl(Subsequence lb,shape_t e, Subsequence rloc) {
    return e;
  }

  shape_t edr(Subsequence lloc, shape_t e,Subsequence rb) {
    return e;
  }

  shape_t edlr(Subsequence lb,shape_t e,Subsequence rb) {
    return e;
  }

  shape_t drem(Subsequence lloc, shape_t e, Subsequence rloc) {
    return e;
  }

  shape_t sr(Subsequence lb,shape_t e,Subsequence rb) {
    return e;
  }

  shape_t hl(Subsequence lb,Subsequence region,Subsequence rb) {
    return shape_t('[') + ']';
  }


  shape_t bl(Subsequence lb,Subsequence lregion,shape_t e,Subsequence rb) {
    return e;
  }

  shape_t br(Subsequence lb,shape_t e,Subsequence rregion,Subsequence rb) {
    return e;
  }

  shape_t il(Subsequence lb,Subsequence lregion,shape_t e,Subsequence rregion,Subsequence rb) {
    return e;
  }

  shape_t ml(Subsequence lb,shape_t e,Subsequence rb) {
    return '[' + e + ']';
  }

  shape_t mldr(Subsequence lb,shape_t e,Subsequence dr,Subsequence rb) {
    return '[' + e + ']';
  }

  shape_t mladr(Subsequence lb,shape_t e,Subsequence dr,Subsequence rb) {
    return '[' + e+ ']';
  }

  shape_t mldlr(Subsequence lb,Subsequence dl,shape_t e,Subsequence dr,Subsequence rb) {
    return '[' + e+ ']';
  }

  shape_t mladlr(Subsequence lb,Subsequence dl,shape_t e,Subsequence dr,Subsequence rb) {
    return '[' + e+ ']';
  }

  shape_t mldladr(Subsequence lb,Subsequence dl,shape_t e,Subsequence dr,Subsequence rb) {
    return '[' + e+ ']';
  }

  shape_t mladldr(Subsequence lb,Subsequence dl,shape_t e,Subsequence dr,Subsequence rb) {
    return '[' + e+ ']';
  }

  shape_t mldl(Subsequence lb,Subsequence dl,shape_t e,Subsequence rb) {
    return '[' + e+ ']';
  }

  shape_t mladl(Subsequence lb,Subsequence dl,shape_t e,Subsequence rb) {
    return '[' + e+ ']';
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
    return le + re;
  }

  shape_t acomb(shape_t le,Subsequence b,shape_t re) {
    return le + re;
  }

  choice [shape_t] h([shape_t] i) {
    return unique(i);
  }
}

algebra alg_shape4 extends alg_shape5 {
  shape_t il(Subsequence lb,Subsequence lregion,shape_t e,Subsequence rregion,Subsequence rb) {
    return shape_t('[') + e + ']';
  }
}

algebra alg_shape3 extends alg_shape5 {
  shape_t bl(Subsequence lb,Subsequence lregion,shape_t e,Subsequence rb) {
    return shape_t('[') + e + ']';
  }

  shape_t br(Subsequence lb,shape_t e,Subsequence rregion,Subsequence rb) {
    return '[' + e + ']';
  }
  
  shape_t il(Subsequence lb,Subsequence lregion,shape_t e,Subsequence rregion,Subsequence rb) {
    return shape_t('[') + e + ']';
  }
}

algebra alg_shape2 extends alg_shape5 {
  shape_t bl(Subsequence lb,Subsequence lregion,shape_t e,Subsequence rb) {
    return shape_t('[') + '_' + e + ']';
  }

  shape_t br(Subsequence lb,shape_t e,Subsequence rregion,Subsequence rb) {
    return '[' + e + '_' + ']';
  }

  shape_t il(Subsequence lb,Subsequence lregion,shape_t e,Subsequence rregion,Subsequence rb) {
    return shape_t('[') + '_' + e + '_' + ']';
  }
}

algebra alg_shape1 extends alg_shape5 {
  shape_t sadd(Subsequence b, shape_t e) {
    if (front(e) == '_') {
      return e;
    } else {
      return '_' + e;
    }
  }

  shape_t cadd(shape_t x, shape_t y) {
    if (back(x) == '_' && front(y) == '_') {
      return x + tail(y);
    } else {
      return x + y; //not possible in macrostates, because there y has always a at least a single unpaired base at its left
    }
  }
  shape_t cadd_Pr_Pr(shape_t le,shape_t re) {
    return le + tail(re);
  }

  shape_t ambd(shape_t le,Subsequence b,shape_t re) {
    return le + '_' + re;
  }

  shape_t ambd_Pr(shape_t le,Subsequence b,shape_t re) {
    return le + '_' + re;
  }

  shape_t edl(Subsequence lb,shape_t e, Subsequence rloc) {
    return '_' + e;
  }

  shape_t edr(Subsequence lloc, shape_t e,Subsequence rb) {
    return e + '_';
  }

  shape_t edlr(Subsequence lb,shape_t e,Subsequence rb) {
    return '_' + e + '_';
  }

  shape_t bl(Subsequence lb,Subsequence lregion,shape_t e,Subsequence rb) {
    return shape_t('[') + '_' + e + ']';
  }

  shape_t br(Subsequence lb,shape_t e,Subsequence rregion,Subsequence rb) {
    return '[' + e + '_' + ']';
  }

  shape_t il(Subsequence lb,Subsequence lregion,shape_t e,Subsequence rregion,Subsequence rb) {
    return shape_t('[') + '_' + e + '_' + ']';
  }

  shape_t mladr(Subsequence lb,shape_t e,Subsequence dr,Subsequence rb) {
    return '[' + e + '_' + ']';
  }

  shape_t mldr(Subsequence lb,shape_t e,Subsequence dr,Subsequence rb) {
    if (back(e) == '_') {
      return shape_t('[') + e + shape_t(']');
    } else {
      return shape_t('[') + e + shape_t('_') + shape_t(']'); //cannot happen in macrostates, because this is handled in the mladr case
    }
  }

  shape_t mladlr(Subsequence lb,Subsequence dl,shape_t e,Subsequence dr,Subsequence rb) {
    return shape_t('[') + '_' + e + '_' + ']';
  }

  shape_t mldladr(Subsequence lb,Subsequence dl,shape_t e,Subsequence dr,Subsequence rb) {
    return '[' + e + '_' + ']';
  }

  shape_t mladldr(Subsequence lb,Subsequence dl,shape_t e,Subsequence dr,Subsequence rb) {
    return shape_t('[') + '_' + e + ']';
  }

  shape_t mldl(Subsequence lb,Subsequence dl,shape_t e,Subsequence rb) {
    if (front(e) == '_') {
      return shape_t('[') + e + shape_t(']');
    } else {
      return shape_t('[') + shape_t('_') + e + shape_t(']'); //cannot happen in macrostates, because this is handled in the mladl case
    }
  }

  shape_t mladl(Subsequence lb,Subsequence dl,shape_t e,Subsequence rb) {
    return shape_t('[') + '_' + e + ']';
  }

  shape_t mldlr(Subsequence lb,Subsequence dl,shape_t x,Subsequence dr,Subsequence rb) {
    shape_t res;
    if (front(x) == '_') {
      res = x;
    } else {
      res = shape_t('_') + x; //cannot happen in macrostates
    }
    if (back(res) != '_') {
      res = res + shape_t('_'); //cannot happen in macrostates
    }
    return shape_t('[') + res + shape_t(']');
  }
  
  shape_t combine(shape_t le,shape_t re) {
    if (back(le) == '_' && front(re) == '_') {
      return le + tail(re);
    } else {
      return le + re;
    }
  }

  shape_t acomb(shape_t le,Subsequence b,shape_t re) {
    return le + '_' + re;
  }
  
  shape_t addss(shape_t x,Subsequence rb) {
    if (back(x) == '_') {
      return x;
    } else {
      return x + shape_t('_'); //cannot happen in macrostates, because we know that x has at least one unpaired base and thus we already have the '_'
    }
  }
}


algebra alg_shape5rope implements sig_foldrna(alphabet = char, answer = Rope) {
  Rope sadd(Subsequence b, Rope e) {
    Rope emptyShape;
    
    if (e == emptyShape) {
      Rope res;
      append(res, '_');
		append(res, e);
      return res;
    } else {
      return e;
    }
  }

  Rope cadd(Rope le,Rope re) {
    if (re == "_") {
      return le;
    } else {
		Rope res;
		append(res, le);
		append(res, re);
      return res;
    }
  }

  Rope cadd_Pr(Rope le,Rope re) {
		Rope res;
		append(res, le);
		append(res, re);
      return res;
  }

  Rope cadd_Pr_Pr(Rope le,Rope re) {
    if (re == "_") {
      return le;
    } else {
		Rope res;
		append(res, le);
		append(res, re);
      return res;
    }
  }

  Rope cadd_Pr_Pr_Pr(Rope le,Rope re) {
		Rope res;
		append(res, le);
		append(res, re);
      return res;
  }

  Rope ambd(Rope le,Subsequence b,Rope re) {
		Rope res;
		append(res, le);
		append(res, re);
      return res;
  }

  Rope ambd_Pr(Rope le,Subsequence b,Rope re) {
		Rope res;
		append(res, le);
		append(res, re);
      return res;
  }

  Rope nil(Subsequence loc) {
    Rope r;
    return r;
  }

  Rope edl(Subsequence lb,Rope e, Subsequence rloc) {
    return e;
  }

  Rope edr(Subsequence lloc, Rope e,Subsequence rb) {
    return e;
  }

  Rope edlr(Subsequence lb,Rope e,Subsequence rb) {
    return e;
  }

  Rope drem(Subsequence lloc, Rope e, Subsequence rloc) {
    return e;
  }

  Rope sr(Subsequence lb,Rope e,Subsequence rb) {
    return e;
  }

  Rope hl(Subsequence lb,Subsequence region,Subsequence rb) {
	  Rope res;
	  append(res, "[]", 2);
	  return res;
  }


  Rope bl(Subsequence lb,Subsequence lregion,Rope e,Subsequence rb) {
    return e;
  }

  Rope br(Subsequence lb,Rope e,Subsequence rregion,Subsequence rb) {
    return e;
  }

  Rope il(Subsequence lb,Subsequence lregion,Rope e,Subsequence rregion,Subsequence rb) {
    return e;
  }

  Rope ml(Subsequence lb,Rope e,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, ']');
    return res;
  }

  Rope mldr(Subsequence lb,Rope e,Subsequence dr,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, ']');
    return res;
  }

  Rope mladr(Subsequence lb,Rope e,Subsequence dr,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, ']');
    return res;
  }

  Rope mldlr(Subsequence lb,Subsequence dl,Rope e,Subsequence dr,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, ']');
    return res;
  }

  Rope mladlr(Subsequence lb,Subsequence dl,Rope e,Subsequence dr,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, ']');
    return res;
  }

  Rope mldladr(Subsequence lb,Subsequence dl,Rope e,Subsequence dr,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, ']');
    return res;
  }

  Rope mladldr(Subsequence lb,Subsequence dl,Rope e,Subsequence dr,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, ']');
    return res;
  }

  Rope mldl(Subsequence lb,Subsequence dl,Rope e,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, ']');
    return res;
  }

  Rope mladl(Subsequence lb,Subsequence dl,Rope e,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, ']');
    return res;
  }

  Rope addss(Rope e,Subsequence rb) {
    return e;
  }

  Rope ssadd(Subsequence lb,Rope e) {
    return e;
  }

  Rope trafo(Rope e) {
    return e;
  }

  Rope incl(Rope e) {
    return e;
  }

  Rope combine(Rope le,Rope re) {
		Rope res;
		append(res, le);
		append(res, re);
    return res;
  }

  Rope acomb(Rope le,Subsequence b,Rope re) {
		Rope res;
		append(res, le);
		append(res, re);
    return res;
  }

  choice [Rope] h([Rope] i) {
    return unique(i);
  }
}

algebra alg_shape4rope extends alg_shape5rope {
  Rope il(Subsequence lb,Subsequence lregion,Rope e,Subsequence rregion,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, ']');
    return res;
  }
}

algebra alg_shape3rope extends alg_shape5rope {
  Rope bl(Subsequence lb,Subsequence lregion,Rope e,Subsequence rb) {
    Rope res;
    append(res, '[');
    append(res, e);
    append(res, ']');
    return res;
  }

  Rope br(Subsequence lb,Rope e,Subsequence rregion,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, ']');
    return res;
  }
  
  Rope il(Subsequence lb,Subsequence lregion,Rope e,Subsequence rregion,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, ']');
    return res;
  }
}

algebra alg_shape2rope extends alg_shape5rope {
  Rope bl(Subsequence lb,Subsequence lregion,Rope e,Subsequence rb) {
    Rope res;
    append(res, '[');
    append(res, '_');
    append(res, e);
    append(res, ']');
    return res;
  }

  Rope br(Subsequence lb,Rope e,Subsequence rregion,Subsequence rb) {
    Rope res;
    append(res, '[');
    append(res, e);
    append(res, '_');
    append(res, ']');
    return res;
  }

  Rope il(Subsequence lb,Subsequence lregion,Rope e,Subsequence rregion,Subsequence rb) {
    Rope res;
    append(res, '[');
    append(res, '_');
    append(res, e);
    append(res, '_');
    append(res, ']');
    return res;
  }
}

algebra alg_shape1rope extends alg_shape5rope {
  Rope sadd(Subsequence b, Rope e) {
    if (front(e) == '_') {
      return e;
    } else {
		Rope res;
		append(res, '_');
		append(res, e);
      return res;
    }
  }

  Rope cadd(Rope x, Rope y) {
    if (back(x) == '_' && front(y) == '_') {
		Rope res;
		append(res, x);
		append(res, tail(y));
      return res;
    } else {
		Rope res;
		append(res, x);
		append(res, y);
      return res; //not possible in macrostates, because there y has always a at least a single unpaired base at its left
    }
  }
  Rope cadd_Pr_Pr(Rope le,Rope re) {
	  Rope res;
	  append(res, le);
	  append(res, tail(re));
    return res;
  }

  Rope ambd(Rope le,Subsequence b,Rope re) {
	  Rope res;
	  append(res, le);
	  append(res, '_');
	  append(res, re);
    return res;
  }

  Rope ambd_Pr(Rope le,Subsequence b,Rope re) {
	  Rope res;
	  append(res, le);
	  append(res, '_');
	  append(res, re);
    return res;
  }

  Rope edl(Subsequence lb,Rope e, Subsequence rloc) {
	  Rope res;
	  append(res, '_');
	  append(res, e);
    return res;
  }

  Rope edr(Subsequence lloc, Rope e,Subsequence rb) {
	  Rope res;
	  append(res, e);
	  append(res, '_');
    return res;
  }

  Rope edlr(Subsequence lb,Rope e,Subsequence rb) {
	  Rope res;
	  append(res, '_');
	  append(res, e);
	  append(res, '_');
    return res;
  }

  Rope bl(Subsequence lb,Subsequence lregion,Rope e,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, '_');
	  append(res, e);
	  append(res, ']');
    return res;
  }

  Rope br(Subsequence lb,Rope e,Subsequence rregion,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, '_');
	  append(res, ']');
    return res;
  }

  Rope il(Subsequence lb,Subsequence lregion,Rope e,Subsequence rregion,Subsequence rb) {
    Rope res;
    append(res, '[');
    append(res, '_');
    append(res, e);
    append(res, '_');
    append(res, ']');
    return res;
  }

  Rope mladr(Subsequence lb,Rope e,Subsequence dr,Subsequence rb) {
	  Rope res;
	  append(res, '[');
	  append(res, e);
	  append(res, '_');
	  append(res, ']');
    return res;
  }

  Rope mldr(Subsequence lb,Rope e,Subsequence dr,Subsequence rb) {
    Rope res;
    append(res, '[');
    append(res, e);
    if (back(e) != '_') {
      append(res, '_');
    }
    append(res, ']');
    return res;
  }

  Rope mladlr(Subsequence lb,Subsequence dl,Rope e,Subsequence dr,Subsequence rb) {
    Rope res;
    append(res, '[');
    append(res, '_');
    append(res, e);
    append(res, '_');
    append(res, ']');
    return res;
  }

  Rope mldladr(Subsequence lb,Subsequence dl,Rope e,Subsequence dr,Subsequence rb) {
    Rope res;
    append(res, '[');
    append(res, e);
    append(res, '_');
    append(res, ']');
    return res;
  }

  Rope mladldr(Subsequence lb,Subsequence dl,Rope e,Subsequence dr,Subsequence rb) {
    Rope res;
    append(res, '[');
    append(res, '_');
    append(res, e);
    append(res, ']');
    return res;
  }

  Rope mldl(Subsequence lb,Subsequence dl,Rope e,Subsequence rb) {
    Rope res;
    append(res, '[');
    if (front(e) != '_') {
      append(res, '_');
    }
	append(res, e);
	append(res, ']');
	return res;
  }

  Rope mladl(Subsequence lb,Subsequence dl,Rope e,Subsequence rb) {
    Rope res;
    append(res, '[');
    append(res, '_');
    append(res, e);
    append(res, ']');
    return res;
  }

  Rope mldlr(Subsequence lb,Subsequence dl,Rope x,Subsequence dr,Subsequence rb) {
    Rope res;
    append(res, '[');
    if (front(x) != '_') {
      append(res, '_');
    }
    append(res, x);
    if (back(res) != '_') {
      append(res, '_');
    }
    append(res, ']');
    return res;
  }
  
  Rope combine(Rope le,Rope re) {
    Rope res;
    append(res, le);
    if (back(le) == '_' && front(re) == '_') {
      append(res, tail(re));
    } else {
      append(res, re);
    }
    return res;
  }

  Rope acomb(Rope le,Subsequence b,Rope re) {
    Rope res;
    append(res, le);
    append(res, '_');
    append(res, re);
    return res;
  }
  
  Rope addss(Rope x,Subsequence rb) {
    if (back(x) == '_') {
      return x;
    } else {
      Rope res;
      append(res, x);
      append(res, '_');
      return res;
    }
  }

}


algebra alg_count auto count ;
algebra alg_enum auto enum ;

//This is the grammar, developed by Bjoern Voss, for the probablistic shape analysis of RNAshapes 2006 release. It is also known as "canonicals_nonamb" in the Haskell version of RNAshapes, or "adpf_nonamb"

//applying "basepair" instead of the build-in "basepairing" or "stackpairing" to be general enough to handle single sequence and alignment predictions. Remember to import singlefold.hh or alifold.hh!
grammar gra_macrostate uses sig_foldrna(axiom = struct) {
  struct = left_dangle | trafo(noleft_dangle) | left_unpaired # h;

  left_unpaired = sadd(BASE, left_unpaired) | sadd(BASE, left_dangle) # h;

  left_dangle = ambd(edanglel, BASE, noleft_dangle) | cadd_Pr(edanglel, {noleft_dangle | nil(LOC)}) | cadd(edanglelr, {left_dangle | left_unpaired}) | nil(LOC) # h;

  noleft_dangle = cadd_Pr_Pr(edangler, {left_dangle | left_unpaired}) | cadd_Pr_Pr_Pr(nodangle, {noleft_dangle | nil(LOC)}) | ambd_Pr(nodangle, BASE, noleft_dangle) # h;

  edanglel = edl(BASE, strong, LOC) # h;

  edangler = edr(LOC, strong, BASE) # h;

  edanglelr = edlr(BASE, strong, BASE) # h;

  nodangle = drem(LOC, strong, LOC) # h;

  strong    = {sr(BASE, weak, BASE) with basepair} with allowLonelyBasepairs(false) | 
			  {		    weak                     } with allowLonelyBasepairs(true)  # h;

  weak = stack | hairpin | multiloop | leftB | rightB | iloop # h;

  multiloop = {mldl   (BASE, BASE, ml_comps1,       BASE) with basepair  | 
               mladl  (BASE, BASE, ml_comps2,       BASE) with basepair  | 
               mldr   (BASE,       ml_comps3, BASE, BASE) with basepair  | 
               mladr  (BASE,       ml_comps2, BASE, BASE) with basepair  | 
               mldlr  (BASE, BASE, ml_comps4, BASE, BASE) with basepair  | 
               mladlr (BASE, BASE, ml_comps2, BASE, BASE) with basepair  | 
               mldladr(BASE, BASE, ml_comps1, BASE, BASE) with basepair  | 
               mladldr(BASE, BASE, ml_comps3, BASE, BASE) with basepair  | 
               ml     (BASE,       ml_comps2,       BASE) with basepair} # h;

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

  stack   = sr(BASE,                          weak,                            BASE) with basepair # h;
  hairpin = hl(BASE,                          REGION with minsize(3),          BASE) with basepair # h;
  leftB   = bl(BASE, REGION with maxsize(30), strong,                          BASE) with basepair # h;
  rightB  = br(BASE,                          strong, REGION with maxsize(30), BASE) with basepair # h;
  iloop   = il(BASE, REGION with maxsize(30), strong, REGION with maxsize(30), BASE) with basepair # h;

}

instance shape1count = gra_macrostate(alg_shape1 * alg_count);
instance shape1ropecount = gra_macrostate(alg_shape1rope * alg_count);
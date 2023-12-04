
signature Algebra(alphabet, sortA, sortB) {
  sortA f(sortB);
  sortB g(Subsequence, Subsequence);
  choice [sortA] h1([sortA]);
  choice [sortB] h2([sortB]);
}

algebra shape implements Algebra(alphabet = char, sortA = shape, sortB = shape)
{
  shape f(shape x) { return x; }
  shape g(Subsequence a, Subsequence b) { shape r; append(r, '_'); return r; }
  choice [shape] h1([shape] l) { return unique(l); }
  choice [shape] h2([shape] l) { return unique(l); }
}


// test case for multiple classifying choice fns -> classify optimization

grammar Grammar uses Algebra(axiom = start) {


  start = A  # h1 ;

  A = f(B) # h1 ;

  B = g(REGION, REGION) # h2 ;

}

instance shape = Grammar(shape) ;

input <raw,raw>

signature Sig(alphabet, answer){
  answer f1(<answer, answer>, answer);
  answer end(<void, void>);
  answer f2(Subsequence, Subsequence);
  choice [answer] h([answer]);
}

algebra count auto count;


grammar mcount uses Sig (axiom = ali) {

  ali = f1(<g, g>, ali) |
        end(<EMPTY, EMPTY>) # h ;

  g = f2(REGION, REGION) # h;
}


instance cnt = mcount(count);
